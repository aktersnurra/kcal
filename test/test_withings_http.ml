let auth store scopes =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client" ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.); scopes } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store) ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let integration ?(sync = fun _ _ -> Ok ()) store =
  let module Client = struct
    let exchange_code ~redirect_uri:_ ~code:_ = Error (Error.Invalid_input "unused")
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")
    let get_measurements ~access_token:_ ~lastupdate:_ = Ok Withings.{ measurements = []; lastupdate = None }
    let subscribe ~access_token:_ ~callback_url:_ = Ok ()
  end in
  Http_adapter.{ oauth = Withings_oauth.make ~store ~now:(fun () -> Option.get (Ptime.of_float_s 1_800_000_000.)); client = (module Client); token_key = Bytes.make 32 'k'; sync; callback_url = "https://kcal.example.com/withings/webhook"; client_id = "client"; redirect_uri = "https://kcal.example.com/withings/callback" }

let request store ?(scopes = []) ?(headers = []) method_ path body =
  Http_adapter.handle_withings ~protected_resource:None ~schedule_sync:(fun sync -> sync ()) ~withings:(Some (integration store)) ~auth:(auth store scopes) ~service:(Service.make ~store) ~method_ ~path ~headers ~body

let test_webhook_boundaries () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "head" 200 (request store `HEAD "/withings/webhook" "").status;
  Alcotest.(check int) "malformed" 400 (request store ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] `POST "/withings/webhook" "{}").status;
  Alcotest.(check int) "unknown identity" 404 (request store ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] `POST "/withings/webhook" "userid=unknown&appli=1").status

let test_known_webhook_schedules_sync_after_validation () =
  let store, _ = Test_support.store_with_user () in
  let user = Result.get_ok (Store_sqlite.resolve_user store ~issuer:"issuer" ~subject:"webhook-user") in
  let connection = Result.get_ok (Store_sqlite.ensure_withings_connection store ~user ~withings_user_id:"known") in
  let sync_calls = ref 0 in
  let scheduled = ref None in
  let withings = integration ~sync:(fun _ _ -> incr sync_calls; Ok ()) store in
  let response = Http_adapter.handle_withings ~protected_resource:None ~schedule_sync:(fun sync -> scheduled := Some sync)
    ~withings:(Some withings) ~auth:(auth store []) ~service:(Service.make ~store)
    ~method_:`POST ~path:"/withings/webhook" ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"userid=known&appli=1" in
  Alcotest.(check int) "acknowledged" 200 response.status;
  Alcotest.(check int) "not in response path" 0 !sync_calls;
  Option.iter (fun sync -> sync ()) !scheduled;
  Alcotest.(check int) "known webhook triggers shared sync" 1 !sync_calls;
  ignore connection

let test_connect_requires_authentication () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "connect auth" 401 (request store `GET "/withings/connect" "").status

let test_connect_unauthorized_carries_challenge () =
  let store, _ = Test_support.store_with_user () in
  let protected_resource =
    Http_adapter.{ resource = "https://kcal.example.com/mcp";
                   authorization_server = "https://id.example.com";
                   metadata_url = "https://kcal.example.com/.well-known/oauth-protected-resource/mcp" }
  in
  let response =
    Http_adapter.handle_withings ~protected_resource:(Some protected_resource)
      ~schedule_sync:(fun sync -> sync ()) ~withings:(Some (integration store))
      ~auth:(auth store []) ~service:(Service.make ~store) ~method_:`GET
      ~path:"/withings/connect" ~headers:[] ~body:""
  in
  Alcotest.(check int) "status" 401 response.status;
  Alcotest.(check (option string)) "challenge"
    (Some {|Bearer resource_metadata="https://kcal.example.com/.well-known/oauth-protected-resource/mcp"|})
    (List.assoc_opt "www-authenticate"
       (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) response.Http_adapter.headers))

let test_connect_requires_withings_manage_scope () =
  let store, _ = Test_support.store_with_user () in
  let headers = [ ("authorization", "Bearer token") ] in
  Alcotest.(check int) "ledger read is insufficient" 403
    (request ~scopes:[ "ledger:read" ] store ~headers `GET "/withings/connect" "").status;
  Alcotest.(check int) "withings manage is accepted" 302
    (request ~scopes:[ "withings:manage" ] store ~headers `GET "/withings/connect" "").status

let test_callback_syncs_before_subscribing () =
  let store, user = Test_support.store_with_user () in
  let events = ref [] in
  let module Client = struct
    let exchange_code ~redirect_uri:_ ~code:_ =
      events := "exchange" :: !events;
      Ok Withings.{ access_token = "access"; refresh_token = "refresh";
                    expires_at = Option.get (Ptime.of_float_s 1_900_000_000.); withings_user_id = "alice" }
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")
    let get_measurements ~access_token:_ ~lastupdate:_ = Error (Error.Invalid_input "unused")
    let subscribe ~access_token:_ ~callback_url:_ = events := "subscribe" :: !events; Ok ()
  end in
  let withings = integration ~sync:(fun _ _ -> events := "sync" :: !events; Ok ()) store in
  let withings = { withings with client = (module Client : Withings.S) } in
  let state, _ = Result.get_ok (Withings_oauth.begin_authorization
    ~client_id:withings.client_id ~redirect_uri:withings.redirect_uri withings.oauth ~user) in
  let response = Http_adapter.handle_withings ~protected_resource:None
    ~schedule_sync:(fun sync -> sync ()) ~withings:(Some withings) ~auth:(auth store [])
    ~service:(Service.make ~store) ~method_:`GET
    ~path:("/withings/callback?state=" ^ state ^ "&code=valid") ~headers:[] ~body:"" in
  Alcotest.(check int) "callback succeeds" 200 response.status;
  Alcotest.(check (list string)) "exchange, sync, subscribe" [ "exchange"; "sync"; "subscribe" ] (List.rev !events)

let test_callback_stops_before_subscription_on_sync_failure () =
  let store, user = Test_support.store_with_user () in
  let subscribed = ref false in
  let module Client = struct
    let exchange_code ~redirect_uri:_ ~code:_ =
      Ok Withings.{ access_token = "access"; refresh_token = "refresh";
                    expires_at = Option.get (Ptime.of_float_s 1_900_000_000.); withings_user_id = "alice" }
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")
    let get_measurements ~access_token:_ ~lastupdate:_ = Error (Error.Invalid_input "unused")
    let subscribe ~access_token:_ ~callback_url:_ = subscribed := true; Ok ()
  end in
  let withings = integration ~sync:(fun _ _ -> Error (Error.Invalid_input "sync failed")) store in
  let withings = { withings with client = (module Client : Withings.S) } in
  let state, _ = Result.get_ok (Withings_oauth.begin_authorization
    ~client_id:withings.client_id ~redirect_uri:withings.redirect_uri withings.oauth ~user) in
  let response = Http_adapter.handle_withings ~protected_resource:None
    ~schedule_sync:(fun sync -> sync ()) ~withings:(Some withings) ~auth:(auth store [])
    ~service:(Service.make ~store) ~method_:`GET
    ~path:("/withings/callback?state=" ^ state ^ "&code=valid") ~headers:[] ~body:"" in
  Alcotest.(check int) "sync failure" 502 response.status;
  Alcotest.(check bool) "subscription skipped" false !subscribed

let () = Alcotest.run "withings http" [ ("routes", [ Alcotest.test_case "webhook boundaries" `Quick test_webhook_boundaries; Alcotest.test_case "known webhook schedules sync" `Quick test_known_webhook_schedules_sync_after_validation; Alcotest.test_case "connect authentication" `Quick test_connect_requires_authentication; Alcotest.test_case "connect unauthorized carries challenge" `Quick test_connect_unauthorized_carries_challenge; Alcotest.test_case "connect requires Withings manage scope" `Quick test_connect_requires_withings_manage_scope; Alcotest.test_case "callback syncs before subscribing" `Quick test_callback_syncs_before_subscribing; Alcotest.test_case "callback stops on sync failure" `Quick test_callback_stops_before_subscription_on_sync_failure ]) ]
