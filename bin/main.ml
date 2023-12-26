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

      let get req =
        let _room_id = Yume.Server.param ":id" req in
        let resp = `Assoc [ ("roomName", `String "room1") ] in
        respond_yojson resp
    end

    module Game = struct
      let get req =
        let room_id = Yume.Server.param ":id" req in
        let resp =
          `Assoc
            [
              ("roomId", `String room_id);
              ("phase", `Int 3);
              ("turn", `String "Charlie");
              ( "nowPosition",
                `List
                  [
                    `Assoc [ ("name", `String "Alice") ];
                    `Assoc [ ("name", `String "Bob"); ("position", `Int 15) ];
                    `Assoc
                      [ ("name", `String "Charlie"); ("position", `Int 130) ];
                    `Assoc [ ("name", `String "Dave"); ("position", `Int 105) ];
                    `Assoc [ ("name", `String "Eve"); ("position", `Int 112) ];
                    `Assoc [ ("name", `String "Frank"); ("position", `Int 139) ];
                  ] );
              ( "history",
                `List
                  [
                    `Assoc
                      [
                        ("phase", `Int 0);
                        ( "player",
                          `List
                            [
                              `Assoc [ ("name", `String "alice") ];
                              `Assoc
                                [
                                  ("name", `String "Bob");
                                  ("position", `Int 12);
                                  ("selectedTicket", `String "TAXI");
                                ];
                              `Assoc
                                [
                                  ("name", `String "Charlie");
                                  ("position", `Int 123);
                                  ("selectedTicket", `String "BUS");
                                ];
                              `Assoc
                                [
                                  ("name", `String "Dave");
                                  ("position", `Int 101);
                                  ("selectedTicket", `String "UNDERGROUND");
                                ];
                              `Assoc
                                [
                                  ("name", `String "Eve");
                                  ("position", `Int 112);
                                  ("selectedTicket", `String "TAXI");
                                ];
                              `Assoc
                                [
                                  ("name", `String "Frank");
                                  ("position", `Int 134);
                                  ("selectedTicket", `String "TAXI");
                                ];
                            ] );
                      ];
                  ] );
            ]
        in
        respond_yojson resp
    end
  end
end

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
  let _ = store |> Yorksap.Store.(find Q.query (7, 13)) in
  (match store |> Yorksap.Store.create_table_room with
  | Ok () -> ()
  | Error msg -> failwith msg);

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
            get "" Room.get_root;
            get "/:id" Room.get;
          ];
          scope "/game" [
            get "/:id" Game.get;
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
