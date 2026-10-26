module Domain_error = Error
open Base
module Error = Domain_error

let now () = Option.value_exn (Ptime.of_float_s (Unix.gettimeofday ()))

let with_database (config : Config.database) f =
  match Store_sqlite.open_database ~path:config.path with
  | Error error ->
      Logs.err (fun m ->
          m "database initialization failed: %s" (Error.to_string error));
      1
  | Ok db ->
      Stdlib.Fun.protect
        ~finally:(fun () -> ignore (Store_sqlite.close db))
        (fun () -> f db)

let withings_client env (config : Config.withings) =
  Withings.make ~now
    ~config:
      Withings.
        { client_id = config.client_id; client_secret = config.client_secret }
    ~request:(Withings_transport.make env)

let reconciliation (config : Config.sync) =
  Eio_main.run @@ fun env ->
  with_database config.database (fun store ->
      let client = withings_client env config.withings in
      let sync =
        Withings_sync.make ~store ~client
          ~token_key:config.withings.token_encryption_key ~now
      in
      match
        Withings_reconciliation.sync_all ~store ~sync:(Withings_sync.sync sync)
      with
      | Ok () -> 0
      | Error _ ->
          Logs.err (fun m -> m "Withings synchronization failed");
          1)

let serve (config : Config.server) =
  Eio_main.run @@ fun env ->
  with_database config.database (fun store ->
      let verifier =
        Oidc.make ~issuer:config.oidc_issuer ~audience:config.oidc_audience
          ~client:(Oidc.https_client env) ()
      in
      let auth =
        Auth.make ~resolve_user:(Store_sqlite.resolve_user store) ~verifier
      in
      let withings =
        Option.map config.withings ~f:(fun integration ->
            let client = withings_client env integration in
            let sync =
              Withings_sync.make ~store ~client
                ~token_key:integration.token_encryption_key ~now
            in
            Http_adapter.
              {
                oauth = Withings_oauth.make ~store ~now;
                client;
                token_key = integration.token_encryption_key;
                sync =
                  (fun user connection ->
                    Withings_sync.sync sync ~user ~connection);
                callback_url = config.public_base_url ^ "/withings/webhook";
                client_id = integration.client_id;
                redirect_uri = config.public_base_url ^ "/withings/callback";
              })
      in
      Http_adapter.run ~withings env ~config ~auth (Service.make ~store);
      0)

let command name ~doc param run =
  let open Cmdliner in
  let open Term.Syntax in
  Cmd.v (Cmd.info name ~doc)
    (let+ config = param in
     match config with
     | Error error ->
         Stdlib.Printf.eprintf "%s\n%!" (Config.error_to_string error);
         1
     | Ok config -> run config)

let () =
  Logging.setup ();
  let open Cmdliner in
  Stdlib.exit
    (Cmd.eval'
       ~argv:
         (Array.of_list (Config.normalize_argv (Array.to_list Stdlib.Sys.argv)))
       (Cmd.group
          (Cmd.info "kcal" ~doc:"Portable local calorie ledger")
          [
            command "migrate" ~doc:"Apply SQLite migrations"
              (Config.migrate_param ()) (fun database ->
                with_database database (fun _ -> 0));
            command "serve" ~doc:"Run the kcal HTTP service"
              (Config.serve_param ()) serve;
            command "sync-withings"
              ~doc:"Synchronize connected Withings accounts"
              (Config.sync_param ()) reconciliation;
          ]))
