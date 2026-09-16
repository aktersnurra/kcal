let auth store =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client"; ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.) } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store)
    ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let request store ?(headers = []) method_ path body =
  Http_adapter.handle ~auth:(auth store) ~service:(Service.make ~store) ~method_ ~path ~headers ~body

let test_health_and_unauthenticated_mcp () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "health" 200 (request store `GET "/health" "").status;
  Alcotest.(check int) "MCP bearer required" 401 (request store `POST "/mcp" "{}").status

let test_media_type_and_parse_error () =
  let store, _ = Test_support.store_with_user () in
  let auth_headers = [ ("authorization", "Bearer token") ] in
  Alcotest.(check int) "exact media type" 415
    (request store ~headers:(("content-type", "application/jsonx") :: auth_headers) `POST "/mcp" "{}").status;
  Alcotest.(check int) "parse error" 400
    (request store ~headers:(("content-type", "application/json; charset=utf-8") :: auth_headers) `POST "/mcp" "{").status

let () =
  Alcotest.run "http"
    [ ("boundary", [ Alcotest.test_case "health and bearer" `Quick test_health_and_unauthenticated_mcp; Alcotest.test_case "media type and parse error" `Quick test_media_type_and_parse_error ]) ]
