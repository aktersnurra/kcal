let migrate () =
  match Config.load_from_environment () with
  | Error _ -> prerr_endline "invalid configuration"; 1
  | Ok config ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () -> match Migration.apply_all db with Ok () -> 0 | Error _ -> prerr_endline "migration failed"; 1)

let serve () =
  match Config.load_from_environment () with
  | Error _ -> prerr_endline "invalid configuration"; 1
  | Ok config ->
      Eio_main.run @@ fun env ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          match Migration.apply_all db with
          | Error _ -> prerr_endline "migration failed"; 1
          | Ok () ->
              let store = db in
              let client = { Oidc.discover = (fun ~issuer:_ -> Error ()); fetch_jwks = (fun ~uri:_ -> Error ()) } in
              let verifier = Oidc.make ~issuer:config.oidc_issuer ~audience:config.oidc_audience ~client () in
              let auth = Auth.make ~resolve_user:(Store_sqlite.resolve_user store) ~verifier in
              Http.run env ~config ~auth (Service.make ~store); 0)

let command =
  let open Cmdliner in
  let serve_command = Cmd.v (Cmd.info "serve" ~doc:"Run the kcal HTTP service") Term.(const (fun () -> exit (serve ())) $ const ()) in
  let migrate_command = Cmd.v (Cmd.info "migrate" ~doc:"Apply SQLite migrations") Term.(const (fun () -> exit (migrate ())) $ const ()) in
  Cmd.group (Cmd.info "kcal") [ serve_command; migrate_command ]

let () = exit (Cmdliner.Cmd.eval command)
