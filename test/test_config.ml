open Base

let get = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Config.error_to_string error)

let database () = Config.database ~path:"fixture.sqlite" |> get

let server ?(url = "https://kcal.example.com/api/")
    ?(listener = "127.0.0.1:8080") () =
  Config.server ~database:(database ()) ~listen_address:listener
    ~public_base_url:url ~oidc_issuer:"https://id.example.com/issuer"
    ~oidc_audience:"kcal" ~withings:None

let opaque_audience () =
  let config = server () |> get in
  Alcotest.(check string) "opaque audience" "kcal" config.oidc_audience;
  Alcotest.(check string)
    "resource" "https://kcal.example.com/api/mcp" config.protected_resource;
  Alcotest.(check string)
    "metadata"
    "https://kcal.example.com/.well-known/oauth-protected-resource/api/mcp"
    config.metadata_url

let invalid_fields () =
  List.iter
    [
      "kcal";
      "http://example.com";
      "https://user:password@example.com";
      "https://example.com?token=secret";
      "https://bad host";
      "https://example.com:abc";
      "https://example.com:65536";
      "https://example.com:999999999999999999999999999999";
      "https://example.com:";
      "https://example.com/#fragment";
      "https:///missing";
    ] ~f:(fun url ->
      Alcotest.(check bool)
        "invalid URL" true
        (Result.is_error (server ~url ())));
  List.iter
    [
      "localhost";
      "127.0.0.1:0";
      "127.0.0.1:65536";
      "bad host:80";
      "127.0.0.1:no";
      ":8080";
      ".:8080";
    ] ~f:(fun listener ->
      Alcotest.(check bool)
        "invalid listener" true
        (Result.is_error (server ~listener ())))

let integration () =
  Alcotest.(check bool)
    "absent Withings" true
    (Result.is_ok
       (Config.optional_withings ~client_id:None ~client_secret:None
          ~token_encryption_key:None));
  List.iter
    [
      (Some "id", None, None);
      (None, Some "secret", None);
      (None, None, Some (String.make 64 'a'));
      (Some "id", Some "secret", None);
      (Some "id", None, Some (String.make 64 'a'));
      (None, Some "secret", Some (String.make 64 'a'));
    ]
    ~f:(fun (client_id, client_secret, token_encryption_key) ->
      Alcotest.(check bool)
        "partial Withings" true
        (Result.is_error
           (Config.optional_withings ~client_id ~client_secret
              ~token_encryption_key)));
  Alcotest.(check bool)
    "empty integration fields are absent" true
    (Result.is_ok
       (Config.optional_withings ~client_id:(Some "") ~client_secret:(Some "")
          ~token_encryption_key:(Some "")));
  List.iter
    [ "secret-key"; String.make 64 'z'; String.make 62 'a' ]
    ~f:(fun key ->
      Alcotest.(check bool)
        "bad key" true
        (Result.is_error
           (Config.withings ~client_id:"id" ~client_secret:"client-secret"
              ~token_encryption_key:key)))

let valid_urls () =
  List.iter
    [
      "https://kcal.example.com";
      "HTTPS://KCAL.EXAMPLE.COM/api";
      "https://kcal.example.com:8443/api/nested/";
    ] ~f:(fun url -> ignore (server ~url () |> get))

let valid_listeners () =
  List.iter
    [
      "localhost:8080"; "127.0.0.1:1"; "0.0.0.0:65535"; "[::1]:8080"; "::1:8080";
    ] ~f:(fun listener -> ignore (server ~listener () |> get))

let invalid_issuer () =
  match
    Config.server ~database:(database ()) ~listen_address:"127.0.0.1:8080"
      ~public_base_url:"https://kcal.example.com" ~oidc_issuer:"SECRET_ISSUER"
      ~oidc_audience:"kcal" ~withings:None
  with
  | Ok _ -> Alcotest.fail "invalid issuer accepted"
  | Error error ->
      let message = Config.error_to_string error in
      Alcotest.(check bool)
        "issuer field named" true
        (String.is_substring message ~substring:"KCAL_OIDC_ISSUER");
      Alcotest.(check bool)
        "issuer value not echoed" false
        (String.is_substring message ~substring:"SECRET_ISSUER")

let redaction () =
  let config =
    Config.withings ~client_id:"id" ~client_secret:"SECRET_CLIENT"
      ~token_encryption_key:(String.make 64 'a')
    |> get
  in
  let server_config =
    Config.server ~database:(database ()) ~listen_address:"127.0.0.1:8080"
      ~public_base_url:"https://kcal.example.com"
      ~oidc_issuer:"https://id.example.com" ~oidc_audience:"kcal"
      ~withings:(Some config)
    |> get
  in
  let printed = Sexp.to_string (Config.sexp_of_server server_config) in
  Alcotest.(check bool)
    "nonsecret diagnostics retained" true
    (String.is_substring printed ~substring:"client_id");
  Alcotest.(check bool)
    "secret-safe sexp" false
    (String.is_substring printed ~substring:"SECRET_CLIENT");
  Alcotest.(check bool)
    "key-safe sexp" false
    (String.is_substring printed ~substring:(String.make 64 'a'));
  match
    Config.withings ~client_id:"id" ~client_secret:"SECRET_CLIENT"
      ~token_encryption_key:"SECRET_KEY"
  with
  | Ok _ -> Alcotest.fail "bad key accepted"
  | Error error ->
      let message = Config.error_to_string error in
      Alcotest.(check bool)
        "field named" true
        (String.is_substring message ~substring:"KCAL_TOKEN_ENCRYPTION_KEY");
      Alcotest.(check bool)
        "key redacted" false
        (String.is_substring message ~substring:"SECRET_KEY")

let () =
  Alcotest.run "config"
    [
      ( "constructors",
        [
          Alcotest.test_case "opaque audience and path deployment" `Quick
            opaque_audience;
          Alcotest.test_case "invalid URLs/listeners" `Quick invalid_fields;
          Alcotest.test_case "valid URLs" `Quick valid_urls;
          Alcotest.test_case "IPv4 IPv6 and hostname listeners" `Quick
            valid_listeners;
          Alcotest.test_case "invalid issuer diagnostics" `Quick invalid_issuer;
          Alcotest.test_case "optional and partial integration / keys" `Quick
            integration;
          Alcotest.test_case "secret redaction" `Quick redaction;
          Alcotest.test_case "database-only migration config" `Quick (fun () ->
              ignore (database ()));
          Alcotest.test_case "serve without integration" `Quick (fun () ->
              ignore (server () |> get));
        ] );
    ]
