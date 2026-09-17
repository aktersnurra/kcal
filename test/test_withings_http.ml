let auth store =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client" ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.) } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store) ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let integration store =
  let module Client = struct
    let exchange_code ~code:_ = Error (Error.Invalid_input "unused")
    let refresh ~refresh_token:_ = Error (Error.Invalid_input "unused")
    let get_measurements ~access_token:_ ~lastupdate:_ = Ok Withings.{ measurements = []; lastupdate = None }
    let subscribe ~access_token:_ ~callback_url:_ = Ok ()
  end in
  Http_adapter.{ oauth = Withings_oauth.make ~store ~now:(fun () -> Option.get (Ptime.of_float_s 1_800_000_000.)); client = (module Client); token_key = Bytes.make 32 'k'; sync = (fun _ _ -> Ok ()); callback_url = "https://kcal.example.com/withings/webhook"; client_id = "client"; redirect_uri = "https://kcal.example.com/withings/callback" }

let request store ?(headers = []) method_ path body =
  Http_adapter.handle_withings ~withings:(Some (integration store)) ~auth:(auth store) ~service:(Service.make ~store) ~method_ ~path ~headers ~body

let test_webhook_boundaries () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "head" 200 (request store `HEAD "/withings/webhook" "").status;
  Alcotest.(check int) "malformed" 400 (request store ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] `POST "/withings/webhook" "{}").status;
  Alcotest.(check int) "unknown identity" 404 (request store ~headers:[ ("content-type", "application/x-www-form-urlencoded") ] `POST "/withings/webhook" "userid=unknown&appli=1").status

let test_connect_requires_authentication () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "connect auth" 401 (request store `GET "/withings/connect" "").status

let () = Alcotest.run "withings http" [ ("routes", [ Alcotest.test_case "webhook boundaries" `Quick test_webhook_boundaries; Alcotest.test_case "connect authentication" `Quick test_connect_requires_authentication ]) ]
