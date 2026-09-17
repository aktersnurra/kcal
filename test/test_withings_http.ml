let auth store =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client" ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.) } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store) ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let integration ?(sync = fun _ _ -> Ok ()) store =
  let module Client = struct
    let exchange_code ~code:_ = Error (Error.Invalid_input "unused")
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")
    let get_measurements ~access_token:_ ~lastupdate:_ = Ok Withings.{ measurements = []; lastupdate = None }
    let subscribe ~access_token:_ ~callback_url:_ = Ok ()
  end in
  Http_adapter.{ oauth = Withings_oauth.make ~store ~now:(fun () -> Option.get (Ptime.of_float_s 1_800_000_000.)); client = (module Client); token_key = Bytes.make 32 'k'; sync; callback_url = "https://kcal.example.com/withings/webhook"; client_id = "client"; redirect_uri = "https://kcal.example.com/withings/callback" }

let request store ?(headers = []) method_ path body =
  Http_adapter.handle_withings ~schedule_sync:(fun sync -> sync ()) ~withings:(Some (integration store)) ~auth:(auth store) ~service:(Service.make ~store) ~method_ ~path ~headers ~body

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
  let response = Http_adapter.handle_withings ~schedule_sync:(fun sync -> scheduled := Some sync)
    ~withings:(Some withings) ~auth:(auth store) ~service:(Service.make ~store)
    ~method_:`POST ~path:"/withings/webhook" ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] ~body:"userid=known&appli=1" in
  Alcotest.(check int) "acknowledged" 200 response.status;
  Alcotest.(check int) "not in response path" 0 !sync_calls;
  Option.iter (fun sync -> sync ()) !scheduled;
  Alcotest.(check int) "known webhook triggers shared sync" 1 !sync_calls;
  ignore connection

let test_connect_requires_authentication () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "connect auth" 401 (request store `GET "/withings/connect" "").status

let () = Alcotest.run "withings http" [ ("routes", [ Alcotest.test_case "webhook boundaries" `Quick test_webhook_boundaries; Alcotest.test_case "known webhook schedules sync" `Quick test_known_webhook_schedules_sync_after_validation; Alcotest.test_case "connect authentication" `Quick test_connect_requires_authentication ]) ]
