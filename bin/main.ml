module Handler = struct
  open Yorksap
  open Yume.Server

  let respond_yojson ?status ?(headers = []) y =
    let content_type = (`Content_type, "application/json; charset=utf-8") in
    Yojson.Safe.to_string y |> respond ?status ~headers:(content_type :: headers)

  let respond_error ~status msg =
    `Assoc [ ("error", `String msg) ] |> respond_yojson ~status

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
                                  yojson_of_move ~name:(List.nth users i) move)
                           ) );
                     ])
          in
          `List (phase0 :: phases)
      end

      let get store req =
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
                |> Game_model.Game.of_yojson ~map:Game_data.London.map
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
                                 ( "position",
                                   `Int (agent |> Agent.loc |> Loc.id) )
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
end

let generate_uuid () = Uuidm.(v `V4 |> to_string)

let server () =
  let open Yorksap.Yume.Server in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dsn = try Unix.getenv "DSN" with Not_found -> failwith "set DSN" in
  let store =
    match Yorksap.Store.connect env ~sw { dsn } with
    | Ok x -> x
    | Error msg -> failwith msg
  in
  (match store |> Yorksap.Store.create_table_room with
  | Ok () -> ()
  | Error msg -> failwith msg);
  (match store |> Yorksap.Store.create_table_user with
  | Ok () -> ()
  | Error msg -> failwith msg);

  (* For debug *)
  (let expect_ok = function Ok () -> () | Error msg -> failwith msg in
   let uuid = generate_uuid () in
   let name = "test game " ^ uuid in
   store
   |> Yorksap.Store.insert_room ~uuid ~name ~game:(Yojson.Safe.to_string `Null)
   |> expect_ok;
   store
   |> Yorksap.Store.insert_user ~room_uuid:uuid ~turn:0 ~name:"ゆ〜ざ〜０"
   |> expect_ok;
   store
   |> Yorksap.Store.insert_user ~room_uuid:uuid ~turn:1 ~name:"ゆ〜ざ〜１"
   |> expect_ok;
   store
   |> Yorksap.Store.insert_user ~room_uuid:uuid ~turn:2 ~name:"ゆ〜ざ〜２"
   |> expect_ok;
   store
   |> Yorksap.Store.insert_user ~room_uuid:uuid ~turn:3 ~name:"ゆ〜ざ〜３"
   |> expect_ok;
   store
   |> Yorksap.Store.insert_user ~room_uuid:uuid ~turn:4 ~name:"ゆ〜ざ〜４"
   |> expect_ok;
   store
   |> Yorksap.Store.insert_user ~room_uuid:uuid ~turn:5 ~name:"ゆ〜ざ〜５"
   |> expect_ok;
   let game =
     let map = Yorksap.Game_data.London.map in
     let init_locs =
       [ 138; 50; 53; 198; 155; 100 ]
       |> List.map (fun id -> Yorksap.Game_model.Loc.make ~id ())
     in
     Yorksap.Game_model.Game.(
       make ~init_locs ~map () |> to_yojson |> Yojson.Safe.to_string)
   in
   (match store |> Yorksap.Store.update_game_by_uuid ~uuid ~game with
   | Ok () -> ()
   | Error msg -> failwith msg);
   ());

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
    Handler.
      [
        scope "/api/v1" Api_v1.[
          scope "/room" [
            get "" (Room.get_root store);
            get "/:id" (Room.get store);
          ];
          scope "/game" [
            get "/:id" (Game.get store);
          ];
        ];
      ] [@ocamlformat "disable"]
  in
  let handler =
    Logger.use @@ Cors.use cors @@ Router.use routes default_handler
  in
  start_server env ~sw handler @@ fun () -> ()

let () =
  Yorksap.Logg.setup ();
  Random.self_init ();

  let open Cmdliner in
  let cmd =
    Cmd.(
      group (info "yorksap")
        [ v (info "server") Term.(const server $ const ()) ])
  in
  exit (Cmd.eval cmd)
