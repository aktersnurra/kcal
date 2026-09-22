let test_metadata_url_is_derived_from_audience_path () =
  Alcotest.(check string) "path-specific metadata url"
    "https://kcal.example.com/.well-known/oauth-protected-resource/mcp"
    (Config.protected_resource_metadata_url ~audience:"https://kcal.example.com/mcp")

let test_metadata_url_for_audience_without_path () =
  Alcotest.(check string) "root metadata url"
    "https://kcal.example.com/.well-known/oauth-protected-resource"
    (Config.protected_resource_metadata_url ~audience:"https://kcal.example.com")

let test_metadata_url_ignores_trailing_slash () =
  Alcotest.(check string) "trailing slash is not a path segment"
    "https://kcal.example.com/.well-known/oauth-protected-resource"
    (Config.protected_resource_metadata_url ~audience:"https://kcal.example.com/")

let test_metadata_url_preserves_nested_path () =
  Alcotest.(check string) "nested path is appended verbatim"
    "https://kcal.example.com/.well-known/oauth-protected-resource/api/mcp"
    (Config.protected_resource_metadata_url ~audience:"https://kcal.example.com/api/mcp")

let test_deployed_audience_derivation () =
  Alcotest.(check string) "deployed metadata url"
    "https://kcal.example.com/.well-known/oauth-protected-resource/mcp"
    (Config.protected_resource_metadata_url ~audience:"https://kcal.example.com/mcp")

let () =
  Alcotest.run "config"
    [ ("protected resource metadata url",
       [ Alcotest.test_case "audience path becomes metadata suffix" `Quick test_metadata_url_is_derived_from_audience_path;
         Alcotest.test_case "audience without path" `Quick test_metadata_url_for_audience_without_path;
         Alcotest.test_case "trailing slash" `Quick test_metadata_url_ignores_trailing_slash;
         Alcotest.test_case "nested path" `Quick test_metadata_url_preserves_nested_path;
         Alcotest.test_case "deployed audience" `Quick test_deployed_audience_derivation ]) ]
