module Handler = struct
  open Yorksap
  open Yume.Server

  let respond_yojson ?status ?(headers = []) y =
    let content_type = (`Content_type, "application/json; charset=utf-8") in
    Yojson.Safe.to_string y |> respond ?status ~headers:(content_type :: headers)

  module Api_v1 = struct
    module Room = struct
      let get_root _req =
        let resp =
          `Assoc
            [
              ( "roomlist",
                `List
                  [
                    `Assoc
                      [
                        ("id", `String "aa317643-a121-49e8-a7f9-6698a7a8be31");
                        ("name", `String "room1");
                      ];
                    `Assoc
                      [
                        ("id", `String "efc85b80-8c54-4bf4-a5c0-9855a7952c45");
                        ("name", `String "wakuwaku-yorkland");
                      ];
                    `Assoc
                      [
                        ("id", `String "8a3cba69-a048-409a-a4cb-0f79b3bd1a95");
                        ("name", `String "welcome room");
                      ];
                  ] );
            ]
        in
        respond_yojson resp

      let get req =
        let _room_id = Yume.Server.param ":id" req in
        let resp = `Assoc [ ("roomlist", `String "room1") ] in
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
