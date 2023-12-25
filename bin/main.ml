module Handler = struct
  open Yorksap

  module Root = struct
    let get _req = Yume.Server.respond ~status:`Not_found ""
  end

  module Api_v1 = struct
    module Game = struct
      module Root = struct
        let get_root _req = Yume.Server.respond ~status:`Not_found ""
        let get _req = Yume.Server.respond ~status:`Not_found ""
      end
    end
  end
end

let server () =
  let open Yorksap.Yume.Server in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let routes =
    let open Router in
    Handler.
      [
        get "/" Root.get;
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
