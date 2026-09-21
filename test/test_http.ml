let auth store scopes =
  let claims = Oidc.{ issuer = "https://issuer.example"; subject = "alice"; audience = [ "kcal-client"; ]; expires_at = Option.get (Ptime.of_float_s 2_000_000_000.); scopes } in
  Auth.make ~resolve_user:(Store_sqlite.resolve_user store)
    ~verifier:(Oidc.of_verified_claims (fun _ -> Ok claims))

let request store ?(scopes = []) ?(headers = []) method_ path body =
  Http_adapter.handle ~auth:(auth store scopes) ~service:(Service.make ~store) ~method_ ~path ~headers ~body ()

let protected_resource =
  Http_adapter.{
    resource = "https://kcal.example.com/mcp";
    authorization_server = "https://id.example.com";
    metadata_url = "https://kcal.example.com/.well-known/oauth-protected-resource/mcp";
  }

let expected_metadata =
  {|{"resource":"https://kcal.example.com/mcp","authorization_servers":["https://id.example.com"],"scopes_supported":["ledger:read","ledger:write","withings:manage"]}|}

let expected_challenge =
  {|Bearer resource_metadata="https://kcal.example.com/.well-known/oauth-protected-resource/mcp"|}

let metadata_request store path =
  Http_adapter.handle ~protected_resource ~auth:(auth store [])
    ~service:(Service.make ~store) ~method_:`GET ~path ~headers:[] ~body:"" ()

let test_protected_resource_metadata_alias () =
  let store, _ = Test_support.store_with_user () in
  let response = metadata_request store "/.well-known/oauth-protected-resource" in
  Alcotest.(check int) "status" 200 response.status;
  Alcotest.(check string) "metadata" expected_metadata response.body

let test_protected_resource_metadata_path_specific () =
  let store, _ = Test_support.store_with_user () in
  let response = metadata_request store "/.well-known/oauth-protected-resource/mcp" in
  Alcotest.(check int) "status" 200 response.status;
  Alcotest.(check string) "metadata" expected_metadata response.body

let test_metadata_endpoints_are_identical () =
  let store, _ = Test_support.store_with_user () in
  let alias = metadata_request store "/.well-known/oauth-protected-resource" in
  let path_specific = metadata_request store "/.well-known/oauth-protected-resource/mcp" in
  Alcotest.(check int) "same status" alias.status path_specific.status;
  Alcotest.(check string) "same body" alias.body path_specific.body

let test_unconfigured_protected_resource_is_not_found () =
  let store, _ = Test_support.store_with_user () in
  Alcotest.(check int) "alias" 404
    (request store `GET "/.well-known/oauth-protected-resource" "").status;
  Alcotest.(check int) "path specific" 404
    (request store `GET "/.well-known/oauth-protected-resource/mcp" "").status

let challenge response =
  List.assoc_opt "www-authenticate"
    (List.map (fun (k, v) -> (String.lowercase_ascii k, v)) response.Http_adapter.headers)

let protected_request store ?(headers = []) () =
  Http_adapter.handle ~protected_resource ~auth:(auth store [])
    ~service:(Service.make ~store) ~method_:`POST ~path:"/mcp" ~headers ~body:"{}" ()

let test_missing_bearer_token_is_challenged () =
  let store, _ = Test_support.store_with_user () in
  let response = protected_request store () in
  Alcotest.(check int) "status" 401 response.status;
  Alcotest.(check (option string)) "challenge" (Some expected_challenge) (challenge response)

let test_invalid_bearer_token_is_challenged () =
  let store, _ = Test_support.store_with_user () in
  let rejecting =
    Auth.make ~resolve_user:(Store_sqlite.resolve_user store)
      ~verifier:(Oidc.of_verified_claims (fun _ -> Error ()))
  in
  let response =
    Http_adapter.handle ~protected_resource ~auth:rejecting ~service:(Service.make ~store)
      ~method_:`POST ~path:"/mcp" ~headers:[ ("authorization", "Bearer bogus") ] ~body:"{}" ()
  in
  Alcotest.(check int) "status" 401 response.status;
  Alcotest.(check (option string)) "challenge" (Some expected_challenge) (challenge response)

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

let test_mcp_notification_is_accepted_without_response () =
  let store, _ = Test_support.store_with_user () in
  let headers = [
    ("authorization", "Bearer token");
    ("content-type", "application/json");
  ] in
  let body = {|{"jsonrpc":"2.0","method":"notifications/initialized"}|} in
  let response = request store ~headers `POST "/mcp" body in
  Alcotest.(check int) "accepted" 202 response.status;
  Alcotest.(check string) "empty body" "" response.body

let test_mcp_scope_failures_are_forbidden () =
  let store, _ = Test_support.store_with_user () in
  let headers = [
    ("authorization", "Bearer token");
    ("content-type", "application/json");
  ] in
  let body =
    {|{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"query_meals","arguments":{}}}|}
  in
  Alcotest.(check int) "authenticated without read scope" 403
    (request ~scopes:[] store ~headers `POST "/mcp" body).status;
  Alcotest.(check int) "read scope accepted" 200
    (request ~scopes:[ "ledger:read" ] store ~headers `POST "/mcp" body).status

let () =
  Alcotest.run "http"
    [ ("boundary", [ Alcotest.test_case "protected resource metadata alias" `Quick test_protected_resource_metadata_alias; Alcotest.test_case "path specific protected resource metadata" `Quick test_protected_resource_metadata_path_specific; Alcotest.test_case "metadata endpoints are identical" `Quick test_metadata_endpoints_are_identical; Alcotest.test_case "missing bearer token is challenged" `Quick test_missing_bearer_token_is_challenged; Alcotest.test_case "invalid bearer token is challenged" `Quick test_invalid_bearer_token_is_challenged; Alcotest.test_case "unconfigured protected resource is not found" `Quick test_unconfigured_protected_resource_is_not_found; Alcotest.test_case "health and bearer" `Quick test_health_and_unauthenticated_mcp; Alcotest.test_case "fixed response length" `Quick test_http_response_has_fixed_length; Alcotest.test_case "media type and parse error" `Quick test_media_type_and_parse_error; Alcotest.test_case "notification accepted without response" `Quick test_mcp_notification_is_accepted_without_response; Alcotest.test_case "scope failures are forbidden" `Quick test_mcp_scope_failures_are_forbidden ]) ]
