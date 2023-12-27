let server () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Yorksap.Request_handler.start_http_server env ~sw (fun () -> ())

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
