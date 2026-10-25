module Domain_error = Error
open Base
module Error = Domain_error

let time seconds = Option.value_exn (Ptime.of_float_s seconds)

let claims ?(issuer = "https://issuer.example") ?(subject = "alice")
    ?(audience = [ "kcal-client" ]) ?(expires_at = time 2_000_000_000.)
    ?(scopes = []) () =
  Oidc.{ issuer; subject; audience; expires_at; scopes }

let verifier results = Oidc.of_verified_claims (fun _ -> results)

let signed_token ?(issuer = "https://issuer.example")
    ?(audience = "kcal-client") ?(expires_at = 2_000_000_000) ?(scopes = "")
    ?scopes_json () =
  let key =
    Option.value_exn
      (Result.ok
         (Jose.Jwk.of_priv_json_string
            {|{"kty":"oct","k":"c2VjcmV0","kid":"test","alg":"HS256"}|}))
  in
  let header = Jose.Header.make_header key in
  let scopes_json =
    Option.value scopes_json ~default:(Stdlib.Printf.sprintf "%S" scopes)
  in
  let payload =
    Stdlib.Printf.sprintf
      {|{"iss":"%s","sub":"alice","aud":"%s","exp":%d,"scope":%s}|} issuer
      audience expires_at scopes_json
  in
  Jose.Jws.sign ~header ~payload key
  |> Result.ok |> Option.value_exn |> Jose.Jws.to_string

let oidc ?(max_age_s = Some 60) ~clock () =
  let keys =
    Jose.Jwks.of_string
      {|{"keys":[{"kty":"oct","k":"c2VjcmV0","kid":"test","alg":"HS256"}]}|}
  in
  let fetches = ref 0 in
  let client =
    Oidc.
      {
        discover =
          (fun ~issuer ->
            Ok { issuer; jwks_uri = "https://issuer.example/keys"; max_age_s });
        fetch_jwks =
          (fun ~uri:_ ->
            Int.incr fetches;
            Ok { keys; max_age_s });
      }
  in
  ( Oidc.make ~issuer:"https://issuer.example" ~audience:"kcal-client" ~clock
      ~client (),
    fetches )

let test_oidc_verifies_signed_jwt_and_cache () =
  let now = ref (time 1_000.) in
  let verifier, fetches = oidc ~max_age_s:(Some 1) ~clock:(fun () -> !now) () in
  Alcotest.(check bool)
    "valid signature" true
    (Result.is_ok (verifier (signed_token ())));
  ignore (verifier (signed_token ()));
  Alcotest.(check int) "cached JWKS" 1 !fetches;
  now := time 1_002.;
  ignore (verifier (signed_token ()));
  Alcotest.(check int) "expired cache refetched" 2 !fetches

let test_oidc_preserves_validated_scopes () =
  let verifier, _ = oidc ~clock:(fun () -> time 1_000.) () in
  let token = signed_token ~scopes:"ledger:read withings:manage" () in
  match verifier token with
  | Ok claims ->
      Alcotest.(check (list string))
        "scopes"
        [ "ledger:read"; "withings:manage" ]
        claims.scopes
  | Error () -> Alcotest.fail "valid token rejected"

let test_oidc_preserves_array_scopes () =
  let verifier, _ = oidc ~clock:(fun () -> time 1_000.) () in
  let token =
    signed_token ~scopes_json:{|["ledger:read","withings:manage"]|} ()
  in
  match verifier token with
  | Ok claims ->
      Alcotest.(check (list string))
        "scopes"
        [ "ledger:read"; "withings:manage" ]
        claims.scopes
  | Error () -> Alcotest.fail "valid token rejected"

let test_oidc_rejects_bad_tokens_and_claims () =
  let verifier, _ = oidc ~clock:(fun () -> time 1_000.) () in
  List.iter
    ~f:(fun token ->
      Alcotest.(check bool) "rejected" true (Result.is_error (verifier token)))
    [
      "not-a-jwt";
      signed_token ~expires_at:1 ();
      signed_token ~issuer:"https://other.example" ();
      signed_token ~audience:"other" ();
      "eyJhbGciOiJIUzI1NiIsImtpZCI6Im90aGVyIn0.e30.bad";
    ];
  let token = signed_token () in
  Alcotest.(check bool)
    "tampered" true
    (Result.is_error (verifier (token ^ "x")))

let test_auth_returns_user_and_scopes () =
  Test_support.with_store (fun store _ ->
      let auth =
        Auth.make
          ~resolve_user:(Store_sqlite.resolve_user store)
          ~verifier:(verifier (Ok (claims ~scopes:[ "ledger:read" ] ())))
      in
      match Auth.authenticate_bearer auth "valid-token" with
      | Ok identity ->
          Alcotest.(check bool)
            "read scope" true
            (Auth.has_scope identity "ledger:read");
          Alcotest.(check bool)
            "write scope" false
            (Auth.has_scope identity "ledger:write")
      | Error _ -> Alcotest.fail "valid token rejected")

let test_rejects_expired_token () =
  Test_support.with_store (fun store _ ->
      let auth =
        Auth.make
          ~resolve_user:(Store_sqlite.resolve_user store)
          ~verifier:(verifier (Ok (claims ~expires_at:(time 0.) ())))
      in
      Alcotest.(check bool)
        "unauthorized" true
        (match Auth.authenticate_bearer auth "expired-test-token" with
        | Error Error.Unauthorized -> true
        | _ -> false))

let test_resolves_verified_identity_stably () =
  Test_support.with_store (fun store _ ->
      let auth =
        Auth.make
          ~resolve_user:(Store_sqlite.resolve_user store)
          ~verifier:(verifier (Ok (claims ())))
      in
      let first =
        Option.value_exn
          (Result.ok (Auth.authenticate_bearer auth "valid-token"))
      in
      let second =
        Option.value_exn
          (Result.ok (Auth.authenticate_bearer auth "valid-token"))
      in
      Alcotest.(check string)
        "stable user"
        (User_id.to_string first.user.id)
        (User_id.to_string second.user.id))

let test_rejects_unverified_token () =
  Test_support.with_store (fun store _ ->
      let auth =
        Auth.make
          ~resolve_user:(Store_sqlite.resolve_user store)
          ~verifier:(verifier (Error ()))
      in
      Alcotest.(check bool)
        "unauthorized" true
        (match Auth.authenticate_bearer auth "malformed-token" with
        | Error Error.Unauthorized -> true
        | _ -> false))

let test_rejects_wrong_issuer_or_audience () =
  Test_support.with_store (fun store _ ->
      let wrong_issuer =
        Auth.make
          ~resolve_user:(Store_sqlite.resolve_user store)
          ~verifier:(verifier (Ok (claims ~issuer:"https://other.example" ())))
      in
      let wrong_audience =
        Auth.make
          ~resolve_user:(Store_sqlite.resolve_user store)
          ~verifier:(verifier (Ok (claims ~audience:[ "other-client" ] ())))
      in
      let unauthorized auth =
        match Auth.authenticate_bearer auth "valid-signature-wrong-claims" with
        | Error Error.Unauthorized -> true
        | _ -> false
      in
      Alcotest.(check bool) "issuer" true (unauthorized wrong_issuer);
      Alcotest.(check bool) "audience" true (unauthorized wrong_audience))

let test_oidc_rejects_malformed_jose_payloads () =
  let verifier, _ = oidc ~clock:(fun () -> time 1_000.) () in
  let encode =
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
  in
  let header = encode {|{"alg":"HS256","kid":"test"}|} in
  List.iter [ "{"; "[]"; "null"; "42" ] ~f:(fun payload ->
      let token = header ^ "." ^ encode payload ^ ".signature" in
      Alcotest.(check bool)
        "malformed JWT rejected" true
        (Result.is_error (verifier token)))

let test_oidc_rejects_malformed_ec_key () =
  let body = {|{"keys":[{"kty":"EC","crv":"P-256","x":"AA","y":"AA"}]}|} in
  let client =
    Oidc.For_testing.client_of_get (fun _ -> Ok (body, Cohttp.Header.init ()))
  in
  Alcotest.(check bool)
    "invalid EC point rejected" true
    (Result.is_error (client.fetch_jwks ~uri:"https://id.example/keys"))

let test_discovery_preserves_issuer_path () =
  List.iter
    ~f:(fun (issuer, expected) ->
      let requested = ref "" in
      let client =
        Oidc.For_testing.client_of_get (fun uri ->
            requested := uri;
            Error ())
      in
      ignore (client.discover ~issuer);
      Alcotest.(check string) "discovery URL" expected !requested)
    [
      ( "https://id.example/tenant",
        "https://id.example/tenant/.well-known/openid-configuration" );
      ( "https://id.example/tenant/",
        "https://id.example/tenant/.well-known/openid-configuration" );
    ]

let test_malformed_oidc_documents () =
  List.iter
    ~f:(fun body ->
      let client =
        Oidc.For_testing.client_of_get (fun _ ->
            Ok (body, Cohttp.Header.init ()))
      in
      Alcotest.(check bool)
        "malformed discovery" true
        (Result.is_error (client.discover ~issuer:"https://id.example"));
      Alcotest.(check bool)
        "malformed keys" true
        (Result.is_error (client.fetch_jwks ~uri:"https://id.example/keys")))
    [ "{"; "[]"; "{}" ]

let test_oidc_transport_failures_and_cancellation () =
  let client =
    Oidc.For_testing.client_of_get (fun _ -> failwith "connection closed")
  in
  Alcotest.(check bool)
    "failing transport" true
    (Result.is_error (client.discover ~issuer:"https://example.test"));
  List.iter
    ~f:(fun exn ->
      let client = Oidc.For_testing.client_of_get (fun _ -> raise exn) in
      Alcotest.check_raises "not swallowed" exn (fun () ->
          ignore (client.fetch_jwks ~uri:"https://example.test/keys")))
    [ Eio.Cancel.Cancelled Stdlib.Exit; Invalid_argument "programming bug" ]

let () =
  Alcotest.run "auth"
    [
      ( "authentication",
        [
          Alcotest.test_case "OIDC signed JWT and cache" `Quick
            test_oidc_verifies_signed_jwt_and_cache;
          Alcotest.test_case "issuer path discovery" `Quick
            test_discovery_preserves_issuer_path;
          Alcotest.test_case "malformed OIDC documents" `Quick
            test_malformed_oidc_documents;
          Alcotest.test_case "malformed JOSE payloads" `Quick
            test_oidc_rejects_malformed_jose_payloads;
          Alcotest.test_case "malformed EC key" `Quick
            test_oidc_rejects_malformed_ec_key;
          Alcotest.test_case "OIDC transport failures and cancellation" `Quick
            test_oidc_transport_failures_and_cancellation;
          Alcotest.test_case "OIDC preserves validated scopes" `Quick
            test_oidc_preserves_validated_scopes;
          Alcotest.test_case "OIDC preserves array scopes" `Quick
            test_oidc_preserves_array_scopes;
          Alcotest.test_case "OIDC rejects malformed and bad claims" `Quick
            test_oidc_rejects_bad_tokens_and_claims;
          Alcotest.test_case "user and scopes" `Quick
            test_auth_returns_user_and_scopes;
          Alcotest.test_case "expired token" `Quick test_rejects_expired_token;
          Alcotest.test_case "stable verified identity" `Quick
            test_resolves_verified_identity_stably;
          Alcotest.test_case "unverified token" `Quick
            test_rejects_unverified_token;
          Alcotest.test_case "wrong issuer or audience" `Quick
            test_rejects_wrong_issuer_or_audience;
        ] );
    ]
