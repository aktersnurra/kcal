let migrate () =
  match Config.load_from_environment () with
  | Error _ -> prerr_endline "invalid configuration"; 1
  | Ok config ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect
        ~finally:(fun () -> ignore (Sqlite3.db_close db))
        (fun () -> match Migration.apply_all db with Ok () -> 0 | Error _ -> prerr_endline "migration failed"; 1)

let withings_client env config =
  let certificates =
    try
      let channel = open_in_bin "/etc/ssl/cert.pem" in
      Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () -> X509.Certificate.decode_pem_multiple (really_input_string channel (in_channel_length channel)))
    with _ -> Error (`Msg "certificate bundle unavailable")
  in
  let request =
    match certificates with
    | Error _ -> { Withings.post_form = (fun ~uri:_ ~fields:_ -> Error (Error.Invalid_input "Withings transport unavailable")) }
    | Ok certificates ->
        let authenticator = X509.Authenticator.chain_of_trust ~time:(fun () -> Ptime.of_float_s (Unix.gettimeofday ())) certificates in
        (match Tls.Config.client ~authenticator () with
        | Error _ -> { Withings.post_form = (fun ~uri:_ ~fields:_ -> Error (Error.Invalid_input "Withings transport unavailable")) }
        | Ok tls ->
            let https uri flow =
              match Uri.host uri with
              | Some hostname -> (match Domain_name.of_string hostname with
                  | Ok host -> Tls_eio.client_of_flow tls ~host:(Domain_name.host_exn host) flow
                  | Error _ -> failwith "invalid Withings HTTPS host")
              | None -> failwith "invalid Withings HTTPS host"
            in
            let client = Cohttp_eio.Client.make ~https:(Some https) env#net in
            { Withings.post_form = (fun ~uri ~fields ->
                try
                  let target = Uri.of_string uri in
                  if Uri.scheme target <> Some "https" then Error (Error.Invalid_input "Withings transport unavailable")
                  else Eio.Switch.run @@ fun sw ->
                    let body = Cohttp_eio.Body.of_string (Uri.encoded_of_query (List.map (fun (key, value) -> (key, [ value ])) fields)) in
                    let headers = Cohttp.Header.init_with "content-type" "application/x-www-form-urlencoded" in
                    let response, response_body = Cohttp_eio.Client.post client ~sw ~headers ~body target in
                    if Cohttp.Code.code_of_status (Cohttp.Response.status response) / 100 <> 2 then Error (Error.Invalid_input "Withings request failed")
                    else Ok (Eio.Flow.read_all response_body)
                with _ -> Error (Error.Invalid_input "Withings request failed")) })
  in
  Withings.make ~config:Withings.{ client_id = config.Config.withings_client_id; client_secret = config.withings_client_secret } ~request

let reconciliation () =
  match Config.load_from_environment () with
  | Error _ -> prerr_endline "invalid configuration"; 1
  | Ok config ->
      Eio_main.run @@ fun env ->
      let db = Sqlite3.db_open config.database_path in
      Fun.protect ~finally:(fun () -> ignore (Sqlite3.db_close db)) (fun () ->
        match Migration.apply_all db with
        | Error _ -> prerr_endline "migration failed"; 1
        | Ok () ->
            let client = withings_client env config in
            let sync = Withings_sync.make ~store:db ~client ~token_key:config.token_encryption_key ~now:(fun () -> Option.get (Ptime.of_float_s (Unix.gettimeofday ()))) in
            match Withings_reconciliation.sync_all ~store:db ~sync:(Withings_sync.sync sync) with
            | Ok () -> 0 | Error _ -> prerr_endline "Withings synchronization failed"; 1)

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

let () = exit (Cmdliner.Cmd.eval command)
