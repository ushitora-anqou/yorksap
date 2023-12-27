let respond_yojson ?status ?(headers = []) y =
  let content_type = (`Content_type, "application/json; charset=utf-8") in
  Yojson.Safe.to_string y
  |> Yume.Server.respond ?status ~headers:(content_type :: headers)

let respond_error ~status msg =
  `Assoc [ ("error", `String msg) ] |> respond_yojson ~status

let to_assoc = Yojson.Safe.Util.to_assoc
let to_string = Yojson.Safe.Util.to_string
let generate_uuid () = Uuidm.(v `V4 |> to_string)

module Api_v1 = struct
  module Room = struct
    let get_root store _req =
      match Store.select_rooms store with
      | Error _ ->
          Logs.debug (fun m -> m "Couldn't select rooms");
          respond_error ~status:`Bad_request "Couldn't select rooms"
      | Ok rooms ->
          let roomlist =
            rooms
            |> List.map (fun (id, name) ->
                   `Assoc [ ("id", `String id); ("name", `String name) ])
          in
          `Assoc [ ("roomlist", `List roomlist) ] |> respond_yojson

    let post_root store req =
      match
        let a =
          req |> Yume.Server.body |> Yojson.Safe.from_string |> to_assoc
        in
        ( List.assoc "roomName" a |> to_string,
          List.assoc "userName" a |> to_string,
          List.assoc "userPassword" a |> to_string )
      with
      | exception _ -> respond_error ~status:`Bad_request "invalid request"
      | room_name, user_name, user_password -> (
          let encrypted_user_password =
            Bcrypt.(hash user_password |> string_of_hash)
          in
          let room_id = generate_uuid () in
          let access_token = Crypto.SecureRandom.unique_token () in
          let game = Yojson.Safe.to_string `Null in
          match
            store
            |> Store.create_room ~room_name ~user_name ~encrypted_user_password
                 ~room_id ~access_token ~game
          with
          | Error msg ->
              Logs.err (fun m -> m "couldn't make a room: %s" msg);
              respond_error ~status:`Internal_server_error
                "couldn't make a room"
          | Ok () ->
              `Assoc
                [
                  ("roomId", `String room_id);
                  ("accessToken", `String access_token);
                ]
              |> respond_yojson)

    let get store req =
      let room_id = Yume.Server.param ":id" req in
      match store |> Store.select_room_by_uuid ~uuid:room_id with
      | Error _ ->
          Logs.debug (fun m -> m "Couldn't get room");
          respond_error ~status:`Bad_request "Couldn't get room"
      | Ok (_uuid, name) ->
          let resp = `Assoc [ ("roomName", `String name) ] in
          respond_yojson resp
  end

  module Game = struct
    module Yojson_of_history = struct
      open Game_model

      let yojson_of_init_loc ~users i = function
        | None ->
            (* hidden loc *)
            `Assoc [ ("name", `String (List.nth users i)) ]
        | Some loc ->
            `Assoc
              [
                ("name", `String (List.nth users i));
                ("position", `Int (Loc.id loc));
              ]

      let split_list n l =
        l
        |> List.fold_left
             (fun acc x ->
               if List.length (List.hd acc) < n then
                 (x :: List.hd acc) :: List.tl acc
               else List.rev (List.hd acc) :: List.tl acc)
             [ [] ]
        |> List.rev

      let string_of_ticket = function
        | `Taxi -> "TAXI"
        | `Bus -> "BUS"
        | `Ug -> "UNDERGROUND"
        | `Secret -> "SECRET"

      let yojson_of_move ~name (move : Move.single_with_hidden option) :
          Yojson.Safe.t =
        let fields = [ ("name", `String name) ] in
        let fields =
          match move with
          | None -> fields
          | Some (`Hidden t) ->
              ("selectedTicket", `String (string_of_ticket t)) :: fields
          | Some (#Move.single as move) ->
              ( "selectedTicket",
                `String (Ticket.of_move_single move |> string_of_ticket) )
              :: ( "position",
                   `Int
                     (Loc.id
                        (match move with
                        | `Taxi loc | `Bus loc | `Ug loc | `Secret loc -> loc))
                 )
              :: fields
        in
        `Assoc fields

      let f ?(num_agents = 6) ~from ~users history =
        let init_locs, moves = history |> History.get_view ~from in
        let phase0 : Yojson.Safe.t =
          `Assoc
            [
              ("phase", `Int 0);
              ( "player",
                `List (init_locs |> List.mapi (yojson_of_init_loc ~users)) );
            ]
        in
        let phases : Yojson.Safe.t list =
          moves
          |> List.fold_left
               (fun acc -> function
                 | `Double (first, second) ->
                     Some second :: None :: None :: None :: None :: None
                     :: Some first :: acc
                 | #Move.single_with_hidden as move -> Some move :: acc)
               []
          |> List.rev |> split_list num_agents
          |> List.mapi (fun i moves ->
                 `Assoc
                   [
                     ("phase", `Int (i + 1));
                     ( "player",
                       `List
                         (moves
                         |> List.mapi (fun i move ->
                                yojson_of_move ~name:(List.nth users i) move))
                     );
                   ])
        in
        `List (phase0 :: phases)
    end

    let get ~game_data ~store req =
      let ( let* ) = Result.bind in
      let room_id = Yume.Server.param ":id" req in
      match
        let* game =
          store
          |> Store.select_game_by_uuid ~uuid:room_id
          |> Result.map Yojson.Safe.from_string
        in
        let* game =
          match game with
          | `Null ->
              (* The room exists, but the game hasn't started yet *)
              Ok None
          | _ ->
              game
              |> Game_model.Game.of_yojson ~map:game_data.Game_data.map
              |> Result.map Option.some
        in
        let* users =
          store |> Store.select_users_by_room_uuid ~room_uuid:room_id
        in
        let* () =
          if
            users
            |> List.mapi (fun i (j, _) -> i = j)
            |> List.fold_left ( && ) true
          then Ok ()
          else Error "corrupted user list"
        in
        let users = users |> List.map snd in
        Ok (game, users)
      with
      | Error msg ->
          Logs.debug (fun m -> m "Couldn't get game: %s" msg);
          respond_error ~status:`Bad_request "couldn't get game"
      | Ok (None, users) ->
          (* The room exists, but the game hasn't started yet *)
          `Assoc
            [
              ("roomId", `String room_id);
              ("phase", `Int (-1));
              ( "nowPosition",
                `List
                  (users
                  |> List.map (fun username ->
                         `Assoc [ ("name", `String username) ])) );
            ]
          |> respond_yojson
      | Ok (Some game, users) ->
          let open Game_model in
          let history = Game.history game in
          let resp =
            `Assoc
              [
                ("roomId", `String room_id);
                ("phase", `Int (Game.turn game));
                ("turn", `String (List.nth users (Game.turn game)));
                ( "nowPosition",
                  `List
                    (List.combine users (Game.agents game)
                    |> List.map (fun (username, agent) ->
                           (* FIXME *)
                           let role = Agent.role agent in
                           let clock = Game.clock game in
                           let fields = [ ("name", `String username) ] in
                           let fields =
                             if
                               role = `Police
                               || role = `MrX
                                  && (clock = 3 || clock = 8 || clock = 13
                                    || clock = 18 || clock = 24)
                             then
                               ("position", `Int (agent |> Agent.loc |> Loc.id))
                               :: fields
                             else fields
                           in
                           `Assoc fields)) );
                ("history", Yojson_of_history.f ~users ~from:`Police history);
              ]
          in
          respond_yojson resp
  end
end

let load_game_data file_path =
  let ic = open_in_bin file_path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  match Yojson.Safe.from_channel ic |> Game_data.of_yojson with
  | Ok x -> x
  | Error msg -> failwith msg

let establish_store env ~sw dsn =
  let store =
    match Store.connect env ~sw { dsn } with
    | Ok x -> x
    | Error msg -> failwith msg
  in
  (match store |> Store.create_table_room with
  | Ok () -> ()
  | Error msg -> failwith msg);
  (match store |> Store.create_table_user with
  | Ok () -> ()
  | Error msg -> failwith msg);
  store

let generate_dummy_data_for_debug ~store ~game_data =
  (* Generate dummy data for debug *)
  (let expect_ok = function Ok () -> () | Error msg -> failwith msg in
   let uuid = generate_uuid () in
   let name = "test game " ^ uuid in
   let encrypted_password = "" in
   let access_token = "" in
   store
   |> Store.insert_room ~uuid ~name ~game:(Yojson.Safe.to_string `Null)
   |> expect_ok;
   store
   |> Store.insert_user ~encrypted_password ~access_token ~room_uuid:uuid
        ~turn:0 ~name:"ゆ〜ざ〜０"
   |> expect_ok;
   store
   |> Store.insert_user ~encrypted_password ~access_token ~room_uuid:uuid
        ~turn:1 ~name:"ゆ〜ざ〜１"
   |> expect_ok;
   store
   |> Store.insert_user ~encrypted_password ~access_token ~room_uuid:uuid
        ~turn:2 ~name:"ゆ〜ざ〜２"
   |> expect_ok;
   store
   |> Store.insert_user ~encrypted_password ~access_token ~room_uuid:uuid
        ~turn:3 ~name:"ゆ〜ざ〜３"
   |> expect_ok;
   store
   |> Store.insert_user ~encrypted_password ~access_token ~room_uuid:uuid
        ~turn:4 ~name:"ゆ〜ざ〜４"
   |> expect_ok;
   store
   |> Store.insert_user ~encrypted_password ~access_token ~room_uuid:uuid
        ~turn:5 ~name:"ゆ〜ざ〜５"
   |> expect_ok;
   let game =
     let map = game_data.Game_data.map in
     let init_locs = game_data |> Game_data.generate_init_locs 6 in
     Game_model.Game.(
       make ~init_locs ~map () |> to_yojson |> Yojson.Safe.to_string)
   in
   (match store |> Store.update_game_by_uuid ~uuid ~game with
   | Ok () -> ()
   | Error msg -> failwith msg);
   ());
  ()

let start_http_server env ~sw k =
  let game_data = load_game_data "london.json" in

  let dsn = try Unix.getenv "DSN" with Not_found -> failwith "set DSN" in
  let store = establish_store env ~sw dsn in
  generate_dummy_data_for_debug ~store ~game_data;

  (* Start HTTP server *)
  let open Yume.Server in
  let cors =
    Cors.
      [
        make "/api/*"
          ~methods:[ `POST; `PUT; `DELETE; `GET; `PATCH; `OPTIONS ]
          ();
      ]
  in
  let routes =
    let open Router in
    [
      scope "/api/v1" Api_v1.[
        scope "/room" [
          get "" (Room.get_root store);
          post "" (Room.post_root store);
          get "/:id" (Room.get store);
        ];
        scope "/game" [
          get "/:id" (Game.get ~game_data ~store);
        ];
      ];
    ] [@ocamlformat "disable"]
  in
  let handler =
    Logger.use @@ Cors.use cors @@ Router.use routes default_handler
  in
  start_server env ~sw handler k
