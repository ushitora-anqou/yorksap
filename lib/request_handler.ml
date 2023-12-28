open Util

let respond_yojson ?status ?(headers = []) y =
  let content_type = (`Content_type, "application/json; charset=utf-8") in
  Yojson.Safe.to_string y
  |> Yume.Server.respond ?status ~headers:(content_type :: headers)

let respond_error ~status msg =
  `Assoc [ ("error", `String msg) ] |> respond_yojson ~status

let to_assoc = Yojson.Safe.Util.to_assoc
let to_string = Yojson.Safe.Util.to_string
let to_int = Yojson.Safe.Util.to_int
let generate_uuid () = Uuidm.(v `V4 |> to_string)

let parse_authorization_header req =
  match req |> Yume.Server.header_opt `Authorization with
  | None -> None
  | Some auth when not (String.starts_with ~prefix:"Bearer " auth) -> None
  | Some auth -> Some (String.sub auth 7 (String.length auth - 7))

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

    let register ~game_data ~store req =
      let room_id = Yume.Server.param ":id" req in
      match store |> Store.select_room_by_uuid ~uuid:room_id with
      | Error _ ->
          Logs.debug (fun m -> m "Couldn't get room");
          respond_error ~status:`Bad_request "Couldn't get room"
      | Ok _ -> (
          match
            let a =
              req |> Yume.Server.body |> Yojson.Safe.from_string |> to_assoc
            in
            ( List.assoc "userName" a |> to_string,
              List.assoc "userPassword" a |> to_string )
          with
          | exception _ -> respond_error ~status:`Bad_request "invalid request"
          | user_name, user_password -> (
              let encrypted_user_password =
                Bcrypt.(hash user_password |> string_of_hash)
              in
              match
                store |> Store.select_users_by_room_uuid ~room_uuid:room_id
              with
              | Error msg ->
                  Logs.err (fun m ->
                      m "couldn't get users by room uuid: %s: %s" room_id msg);
                  respond_error ~status:`Bad_request "couldn't get users"
              | Ok users when List.length users >= 6 ->
                  Logs.err (fun m ->
                      m "couldn't register; already full: %s" room_id);
                  respond_error ~status:`Bad_request
                    "couldn't register; already full"
              | Ok users -> (
                  let turn = List.length users in
                  let access_token = Crypto.SecureRandom.unique_token () in
                  match
                    store
                    |> Store.insert_user ~room_uuid:room_id ~turn
                         ~name:user_name
                         ~encrypted_password:encrypted_user_password
                         ~access_token
                  with
                  | Error msg ->
                      Logs.err (fun m ->
                          m "couldn't insert user: %s: %s: %s" room_id user_name
                            msg);
                      respond_error ~status:`Internal_server_error
                        "couldn't create user"
                  | Ok () ->
                      (if List.length users = 5 then
                         (* Game has started *)
                         let new_game =
                           let map = game_data.Game_data.map in
                           let init_locs =
                             game_data |> Game_data.generate_init_locs 6
                           in
                           Game_model.Game.(
                             make ~init_locs ~map () |> to_yojson
                             |> Yojson.Safe.to_string)
                         in
                         match
                           store
                           |> Store.update_game_by_uuid ~uuid:room_id
                                ~old_game:"null" ~new_game
                         with
                         | Ok () -> ()
                         | Error msg ->
                             Logs.err (fun m ->
                                 m "couldn't update game: %s: %s" room_id msg));
                      `Assoc
                        [
                          ("accessToken", `String access_token);
                          ("turn", `Int turn);
                        ]
                      |> respond_yojson)))

    let login store req =
      let room_id = Yume.Server.param ":id" req in
      match
        let a =
          req |> Yume.Server.body |> Yojson.Safe.from_string |> to_assoc
        in
        ( List.assoc "userName" a |> to_string,
          List.assoc "userPassword" a |> to_string )
      with
      | exception _ -> respond_error ~status:`Bad_request "invalid request"
      | user_name, user_password -> (
          match
            store
            |> Store.select_user_by_room_uuid_and_name ~room_uuid:room_id
                 ~name:user_name
          with
          | Error msg ->
              Logs.err (fun m ->
                  m "couldn't get users by room uuid and user name: %s: %s: %s"
                    room_id user_name msg);
              respond_error ~status:`Bad_request "couldn't get users"
          | Ok (_, encrypted_password, _)
            when not
                   Bcrypt.(
                     verify user_password (hash_of_string encrypted_password))
            ->
              Logs.err (fun m ->
                  m "couldn't verify user password: %s: %s" room_id user_name);
              respond_error ~status:`Unauthorized
                "couldn't verify your password"
          | Ok (turn, _, access_token) ->
              `Assoc
                [ ("accessToken", `String access_token); ("turn", `Int turn) ]
              |> respond_yojson)
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
      let access_token = parse_authorization_header req in
      match
        let* game = store |> Store.select_game_by_uuid ~uuid:room_id in
        let* game =
          try Ok (Yojson.Safe.from_string game)
          with _ -> Error "invalid yojson"
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
        let* logged_user_turn =
          match access_token with
          | None -> Ok None
          | Some access_token ->
              store
              |> Store.select_user_by_room_uuid_and_access_token
                   ~room_uuid:room_id ~access_token
              |> Result.map Option.some
        in
        Ok (game, users, logged_user_turn)
      with
      | Error msg ->
          Logs.debug (fun m -> m "Couldn't get game: %s" msg);
          respond_error ~status:`Bad_request "couldn't get game"
      | Ok (None, users, _) ->
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
      | Ok (Some game, users, logged_user_turn) ->
          let open Game_model in
          let history = Game.history game in
          let resp =
            `Assoc
              [
                ("roomId", `String room_id);
                ("phase", `Int (Game.clock game));
                ("turn", `String (List.nth users (Game.turn game)));
                ("gameOver", `Bool (Game.has_finished game));
                ( "nowPosition",
                  `List
                    (List.combine users (Game.agents game)
                    |> List.map (fun (username, agent) ->
                           let role = Agent.role agent in
                           let clock = Game.clock game in
                           let fields = [ ("name", `String username) ] in
                           let fields =
                             if
                               role = `Police
                               || role = `MrX
                                  && (logged_user_turn = Some 0 || clock = 3
                                    || clock = 8 || clock = 13 || clock = 18
                                    || clock = 24)
                             then
                               ("position", `Int (agent |> Agent.loc |> Loc.id))
                               :: fields
                             else fields
                           in
                           `Assoc fields)) );
                ( "history",
                  Yojson_of_history.f ~users
                    ~from:(if logged_user_turn = Some 0 then `MrX else `Police)
                    history );
                ( "ticket",
                  `List
                    (game |> Game.agents
                    |> List.map (fun a ->
                           let ts = Agent.ticket_set a in
                           `Assoc
                             [
                               ("TAXI", `Int ts.taxi);
                               ("BUS", `Int ts.bus);
                               ("UNDERGROUND", `Int ts.ug);
                               ("SECRET", `Int ts.secret);
                               ("DOUBLE", `Int ts.double);
                             ])) );
                ( "next",
                  if logged_user_turn <> Some (Game.turn game) then `Null
                  else
                    `List
                      (game |> Game.derive_possible_moves
                      |> List.map (fun (m : Move.t) ->
                             let single = function
                               | `Taxi dst ->
                                   `List [ `String "TAXI"; `Int (Loc.id dst) ]
                               | `Bus dst ->
                                   `List [ `String "BUS"; `Int (Loc.id dst) ]
                               | `Ug dst ->
                                   `List
                                     [
                                       `String "UNDERGROUND"; `Int (Loc.id dst);
                                     ]
                               | `Secret dst ->
                                   `List [ `String "SECRET"; `Int (Loc.id dst) ]
                             in
                             match m with
                             | #Move.single as m -> single m
                             | `Double (first, second) ->
                                 `List
                                   [
                                     `String "DOUBLE";
                                     single first;
                                     single second;
                                   ])) );
              ]
          in
          respond_yojson resp

    let parse_move ~ticket ~destination :
        (Game_model.Move.single, string) result =
      let loc = Game_model.Loc.make ~id:destination () in
      match ticket with
      | "TAXI" -> Ok (`Taxi loc)
      | "BUS" -> Ok (`Bus loc)
      | "UNDERGROUND" -> Ok (`Ug loc)
      | "SECRET" -> Ok (`Secret loc)
      | _ -> Error "invalid ticket"

    let handle_move ~game_data ~store ~parse_request req =
      let room_id = Yume.Server.param ":id" req in
      match
        let a =
          req |> Yume.Server.body |> Yojson.Safe.from_string |> to_assoc
        in
        let auth = req |> Yume.Server.header_opt `Authorization in
        let move = parse_request a in
        (move, auth)
      with
      | exception _ -> respond_error ~status:`Bad_request "invalid request"
      | _, None -> respond_error ~status:`Unauthorized "invalid request"
      | _, Some auth when not (String.starts_with ~prefix:"Bearer " auth) ->
          respond_error ~status:`Unauthorized "invalid request"
      | move, Some auth -> (
          let access_token = String.sub auth 7 (String.length auth - 7) in
          match
            let ( let* ) = Result.bind in
            let* user_turn =
              store
              |> Store.select_user_by_room_uuid_and_access_token
                   ~room_uuid:room_id ~access_token
            in
            let* game =
              store
              |> Store.select_game_by_uuid ~uuid:room_id
              |> Result.map Yojson.Safe.from_string
            in
            let* old_game =
              game |> Game_model.Game.of_yojson ~map:game_data.Game_data.map
            in
            let* new_game =
              old_game
              |> Game_model.Game.move_agent move
              |> Result.map_error Game_model.Error.to_string
            in
            Ok (old_game, new_game, user_turn)
          with
          | Error msg ->
              Logs.err (fun m -> m "couldn't get a valid game: %s" msg);
              respond_error ~status:`Bad_request "couldn't get a valid game"
          | Ok (old_game, _, user_turn)
            when Game_model.Game.turn old_game <> user_turn ->
              respond_error ~status:`Bad_request "it's not your turn"
          | Ok (old_game, new_game, _) -> (
              let old_game =
                old_game |> Game_model.Game.to_yojson |> Yojson.Safe.to_string
              in
              let new_game =
                new_game |> Game_model.Game.to_yojson |> Yojson.Safe.to_string
              in
              match
                store
                |> Store.update_game_by_uuid ~uuid:room_id ~old_game ~new_game
              with
              | Ok () -> respond_yojson (`Assoc [])
              | Error msg ->
                  Logs.err (fun m ->
                      m "failed to update game: %s: %s" room_id msg);
                  respond_error ~status:`Internal_server_error
                    "failed to update game"))

    let post_move ~game_data ~store req =
      let parse_request a =
        (parse_move
           ~ticket:(List.assoc "ticket" a |> to_string)
           ~destination:(List.assoc "destination" a |> to_int)
         |> Result.get_ok
          :> Game_model.Move.t)
      in
      handle_move ~game_data ~store ~parse_request req

    let post_double_move ~game_data ~store req =
      let parse_request a =
        let first =
          parse_move
            ~ticket:(List.assoc "ticket1" a |> to_string)
            ~destination:(List.assoc "destination1" a |> to_int)
          |> Result.get_ok
        in
        let second =
          parse_move
            ~ticket:(List.assoc "ticket2" a |> to_string)
            ~destination:(List.assoc "destination2" a |> to_int)
          |> Result.get_ok
        in
        `Double (first, second)
      in
      handle_move ~game_data ~store ~parse_request req
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

let start_http_server env ~sw k =
  let game_data = load_game_data "london.json" in

  let dsn = try Unix.getenv "DSN" with Not_found -> failwith "set DSN" in
  let store = establish_store env ~sw dsn in

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
          post "/:id/register" (Room.register ~store ~game_data);
          post "/:id/login" (Room.login store);
        ];
        scope "/game" [
          get "/:id" (Game.get ~game_data ~store);
          post "/:id/move" (Game.post_move ~game_data ~store);
          post "/:id/double-move" (Game.post_double_move ~game_data ~store);
        ];
      ];
    ] [@ocamlformat "disable"]
  in
  let handler =
    Logger.use @@ Cors.use cors @@ Router.use routes default_handler
  in
  start_server env ~sw handler k
