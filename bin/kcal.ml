let migrate () =
  match Config.load_from_environment () with
  | Error _ -> Logs.err (fun m -> m "invalid configuration"); 1
  | Ok config ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () -> match Migration.apply_all db with Ok () -> 0 | Error _ -> Logs.err (fun m -> m "migration failed"); 1)

let withings_client env config =
  Withings.make
    ~config:Withings.{ client_id = config.Config.withings_client_id; client_secret = config.withings_client_secret }
    ~request:(Withings_transport.make env)

let reconciliation () =
  match Config.load_from_environment () with
  | Error _ -> Logs.err (fun m -> m "invalid configuration"); 1
  | Ok config ->
      Eio_main.run @@ fun env ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
        match Migration.apply_all db with
        | Error _ -> Logs.err (fun m -> m "migration failed"); 1
        | Ok () ->
            let client = withings_client env config in
            let sync = Withings_sync.make ~store:db ~client ~token_key:config.token_encryption_key ~now:(fun () -> Option.get (Ptime.of_float_s (Unix.gettimeofday ()))) in
            match Withings_reconciliation.sync_all ~store:db ~sync:(Withings_sync.sync sync) with
            | Ok () -> 0 | Error _ -> Logs.err (fun m -> m "Withings synchronization failed"); 1)

let serve () =
  match Config.load_from_environment () with
  | Error _ -> Logs.err (fun m -> m "invalid configuration"); 1
  | Ok config ->
      Eio_main.run @@ fun env ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () ->
          match Migration.apply_all db with
          | Error _ -> Logs.err (fun m -> m "migration failed"); 1
          | Ok () ->
              let store = db in
              let client = Oidc.https_client env in
              let verifier = Oidc.make ~issuer:config.oidc_issuer ~audience:config.oidc_audience ~client () in
              let auth = Auth.make ~resolve_user:(Store_sqlite.resolve_user store) ~verifier in
              let client = withings_client env config in
              let sync = Withings_sync.make ~store ~client ~token_key:config.token_encryption_key ~now:(fun () -> Option.get (Ptime.of_float_s (Unix.gettimeofday ()))) in
              let withings = Http_adapter.{ oauth = Withings_oauth.make ~store ~now:(fun () -> Option.get (Ptime.of_float_s (Unix.gettimeofday ()))); client; token_key = config.token_encryption_key; sync = (fun user connection -> Withings_sync.sync sync ~user ~connection); callback_url = config.public_base_url ^ "/withings/webhook"; client_id = config.withings_client_id; redirect_uri = config.public_base_url ^ "/withings/callback" } in
              Http_adapter.run ~withings:(Some withings) env ~config ~auth (Service.make ~store); 0)

let command =
  let open Cmdliner in
  let serve_command = Cmd.v (Cmd.info "serve" ~doc:"Run the kcal HTTP service") Term.(const (fun () -> exit (serve ())) $ const ()) in
  let migrate_command = Cmd.v (Cmd.info "migrate" ~doc:"Apply SQLite migrations") Term.(const (fun () -> exit (migrate ())) $ const ()) in
  let sync_command = Cmd.v (Cmd.info "sync-withings" ~doc:"Synchronize connected Withings accounts") Term.(const (fun () -> exit (reconciliation ())) $ const ()) in
  Cmd.group (Cmd.info "kcal") [ serve_command; migrate_command; sync_command ]

let () =
  Logging.setup ();
  exit (Cmdliner.Cmd.eval command)
