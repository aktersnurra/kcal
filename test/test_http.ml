let auth store scopes =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client"; ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.); scopes } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store)
    ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let request store ?(scopes = []) ?(headers = []) method_ path body =
  Http_adapter.handle ~auth:(auth store scopes) ~service:(Service.make ~store) ~method_ ~path ~headers ~body ()

let test_protected_resource_metadata () =
  let store, _ = Test_support.store_with_user () in
  let response =
    Http_adapter.handle
      ~protected_resource:{ resource = "https://kcal.example.com";
                            authorization_server = "https://id.example.com" }
      ~auth:(auth store []) ~service:(Service.make ~store)
      ~method_:`GET ~path:"/.well-known/oauth-protected-resource"
      ~headers:[] ~body:"" ()
  in
  Alcotest.(check int) "status" 200 response.status;
  Alcotest.(check string) "metadata"
    {|{"resource":"https://kcal.example.com","authorization_servers":["https://id.example.com"],"scopes_supported":["ledger:read","ledger:write","withings:manage"]}|}
    response.body

let test_unconfigured_protected_resource_is_not_found () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "status" 404
    (request store `GET "/.well-known/oauth-protected-resource" "").status

let test_health_and_unauthenticated_mcp () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "health" 200 (request store `GET "/health" "").status;
  Alcotest.(check int) "MCP bearer required" 401 (request store `POST "/mcp" "{}").status

let test_http_response_has_fixed_length () =
  let store, _ = Test_support.store_with_user () in
  let response = Http_adapter.framed_response (request store `GET "/health" "") in
  Alcotest.(check (option string)) "content length" (Some "15")
    (Httpun.Headers.get response.headers "content-length");
  match Httpun.Response.body_length ~request_method:`GET response with
  | `Fixed length -> Alcotest.(check int64) "fixed body length" 15L length
  | _ -> Alcotest.fail "health response is not fixed-length"

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
    [ ("boundary", [ Alcotest.test_case "protected resource metadata" `Quick test_protected_resource_metadata; Alcotest.test_case "unconfigured protected resource is not found" `Quick test_unconfigured_protected_resource_is_not_found; Alcotest.test_case "health and bearer" `Quick test_health_and_unauthenticated_mcp; Alcotest.test_case "fixed response length" `Quick test_http_response_has_fixed_length; Alcotest.test_case "media type and parse error" `Quick test_media_type_and_parse_error; Alcotest.test_case "scope failures are forbidden" `Quick test_mcp_scope_failures_are_forbidden ]) ]
