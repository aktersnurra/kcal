let auth store scopes =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client"; ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.); scopes } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store)
    ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let request store ?(scopes = []) ?(headers = []) method_ path body =
  Http_adapter.handle ?protected_resource:None ~auth:(auth store scopes) ~service:(Service.make ~store) ~method_ ~path ~headers ~body

let test_protected_resource_metadata () =
  let store, _ = Test_support.store_with_user () in
  let response =
    Http_adapter.handle
      ~protected_resource:{ resource = "https://kcal.example.com";
                            authorization_server = "https://id.example.com" }
      ~auth:(auth store []) ~service:(Service.make ~store)
      ~method_:`GET ~path:"/.well-known/oauth-protected-resource"
      ~headers:[] ~body:""
  in
  Alcotest.(check int) "status" 200 response.status;
  Alcotest.(check string) "metadata"
    {|{"resource":"https://kcal.example.com","authorization_servers":["https://id.example.com"],"scopes_supported":["ledger:read","ledger:write","withings:manage"]}|}
    response.body

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

let test_mcp_scope_failures_are_forbidden () =
  let store, _ = Test_support.store_with_user () in
  let headers = [
    ("authorization", "Bearer token");
    ("content-type", "application/json");
  ] in
  let body =
    {|{"jsonrpc":"2.0","method":"tools/call","params":{"name":"query_meals","arguments":{}}}|}
  in
  Alcotest.(check int) "authenticated without read scope" 403
    (request ~scopes:[] store ~headers `POST "/mcp" body).status;
  Alcotest.(check int) "read scope accepted" 200
    (request ~scopes:[ "ledger:read" ] store ~headers `POST "/mcp" body).status

let () =
  Alcotest.run "http"
    [ ("boundary", [ Alcotest.test_case "protected resource metadata" `Quick test_protected_resource_metadata; Alcotest.test_case "health and bearer" `Quick test_health_and_unauthenticated_mcp; Alcotest.test_case "media type and parse error" `Quick test_media_type_and_parse_error; Alcotest.test_case "scope failures are forbidden" `Quick test_mcp_scope_failures_are_forbidden ]) ]
