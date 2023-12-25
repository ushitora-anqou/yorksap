module Handler = struct
  open Yorksap
  open Yume.Server

  let respond_yojson ?status ?(headers = []) y =
    let content_type = (`Content_type, "application/json; charset=utf-8") in
    Yojson.Safe.to_string y |> respond ?status ~headers:(content_type :: headers)

  module Root = struct end

  module Api_v1 = struct
    module Game = struct
      module Root = struct
        let get_root _req =
          `Assoc
            [
              ( "games",
                `List
                  [
                    `Assoc
                      [
                        ("id", `Int 1);
                        ("name", `String "わくわく");
                        ("desc", `String "");
                      ];
                    `Assoc
                      [
                        ("id", `Int 2);
                        ("name", `String "ほげほげ");
                        ("desc", `String "");
                      ];
                    `Assoc
                      [
                        ("id", `Int 3);
                        ("name", `String "ぴよ");
                        ("desc", `String "");
                      ];
                  ] );
            ]
          |> respond_yojson

        let get req =
          match req |> param ":id" |> int_of_string_opt with
          | None ->
              `Assoc [ ("error", `String "invalid game id") ]
              |> respond_yojson ~status:`Not_found
          | Some game_id -> `Assoc [ ("id", `Int game_id) ] |> respond_yojson
      end
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

  let routes =
    let open Router in
    Handler.
      [
        scope "/api/v1" Api_v1.[
          scope "/games" Game.[
            get "" Root.get_root;
            get "/:id" Root.get
          ];
        ];
      ] [@ocamlformat "disable"]
  in
  let handler = Logger.use @@ Router.use routes default_handler in
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
