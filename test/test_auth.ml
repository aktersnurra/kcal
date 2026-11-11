module Domain_error = Error
open Base
module Error = Domain_error

let time seconds = Option.value_exn (Ptime.of_float_s seconds)

let claims ?(issuer = "https://issuer.example") ?(subject = "alice")
    ?(audience = [ "kcal-client" ]) ?(expires_at = time 2_000_000_000.)
    ?(scopes = []) () =
  Oidc.{ issuer; subject; audience; expires_at; scopes }

let verifier results = Oidc.of_verified_claims (fun _ -> results)

let jwk ?(kid = "test") ?(secret = "c2VjcmV0") () =
  Stdlib.Printf.sprintf {|{"kty":"oct","k":"%s","kid":"%s","alg":"HS256"}|}
    secret kid

let jwks keys =
  Jose.Jwks.of_string
    (Stdlib.Printf.sprintf {|{"keys":[%s]}|} (String.concat ~sep:"," keys))

let signed_token ?kid ?secret ?(issuer = "https://issuer.example")
    ?(audience = "kcal-client") ?(expires_at = 2_000_000_000) ?(scopes = "")
    ?scopes_json () =
  let key =
    Option.value_exn
      (Result.ok (Jose.Jwk.of_priv_json_string (jwk ?kid ?secret ())))
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

(* A verifier over a provider whose published keys and availability the test
   controls; [fetches] counts the JWKS requests it receives. *)
let provider ?(max_age_s = Some 60) ?(before_fetch = fun () -> ()) ~clock () =
  let keys = ref (jwks [ jwk () ]) in
  let available = ref true in
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
            before_fetch ();
            if !available then Ok { keys = !keys; max_age_s } else Error ());
      }
  in
  ( Oidc.make ~issuer:"https://issuer.example" ~audience:"kcal-client" ~clock
      ~client (),
    keys,
    available,
    fetches )

let oidc ?max_age_s ~clock () =
  let verifier, _, _, fetches = provider ?max_age_s ~clock () in
  (verifier, fetches)

let test_oidc_refetches_keys_for_an_unknown_key_id () =
  let now = ref (time 1_000.) in
  let verifier, keys, _, fetches = provider ~clock:(fun () -> !now) () in
  ignore (verifier (signed_token ()));
  (* The provider rotates its key well inside the cache lifetime. *)
  keys := jwks [ jwk ~kid:"next" ~secret:"bmV4dA" () ];
  let rotated = signed_token ~kid:"next" ~secret:"bmV4dA" () in
  Alcotest.(check bool)
    "too soon after the last fetch to ask again" true
    (Result.is_error (verifier rotated));
  Alcotest.(check int) "no refetch yet" 1 !fetches;
  now := time 1_061.;
  Alcotest.(check bool)
    "rotated key accepted" true
    (Result.is_ok (verifier rotated));
  Alcotest.(check int) "one refetch" 2 !fetches;
  ignore (verifier (signed_token ~kid:"ghost" ()));
  ignore (verifier (signed_token ~kid:"ghost" ()));
  Alcotest.(check int) "unknown key ids cannot force more refetches" 2 !fetches

let test_oidc_serves_stale_keys_while_the_provider_is_down () =
  let now = ref (time 1_000.) in
  let verifier, _, available, fetches =
    provider ~max_age_s:(Some 1) ~clock:(fun () -> !now) ()
  in
  ignore (verifier (signed_token ()));
  available := false;
  now := time 1_002.;
  Alcotest.(check bool)
    "expired keys still verify" true
    (Result.is_ok (verifier (signed_token ())));
  Alcotest.(check int) "one failed refresh" 2 !fetches;
  now := time 1_003.;
  ignore (verifier (signed_token ()));
  Alcotest.(check int) "the failure is not retried at once" 2 !fetches;
  now := time 1_040.;
  ignore (verifier (signed_token ()));
  Alcotest.(check int) "retried after the cooldown" 3 !fetches;
  now := time 100_000.;
  Alcotest.(check bool)
    "keys a day past expiry are not trusted" true
    (Result.is_error (verifier (signed_token ())))

let test_oidc_does_not_hammer_an_unreachable_provider () =
  let now = ref (time 1_000.) in
  let verifier, _, available, fetches = provider ~clock:(fun () -> !now) () in
  available := false;
  Alcotest.(check bool)
    "no keys, no verification" true
    (Result.is_error (verifier (signed_token ())));
  ignore (verifier (signed_token ()));
  Alcotest.(check int) "one attempt" 1 !fetches;
  available := true;
  now := time 1_031.;
  Alcotest.(check bool)
    "recovers after the cooldown" true
    (Result.is_ok (verifier (signed_token ())))

let test_oidc_concurrent_requests_share_one_refresh () =
  Eio_main.run @@ fun _ ->
  let verifier, _, _, fetches =
    provider ~before_fetch:Eio.Fiber.yield ~clock:(fun () -> time 1_000.) ()
  in
  let verify () = Result.is_ok (verifier (signed_token ())) in
  let first, second = Eio.Fiber.pair verify verify in
  Alcotest.(check bool) "both verified" true (first && second);
  Alcotest.(check int) "one fetch" 1 !fetches

let test_oidc_timeout_is_a_transport_failure () =
  let client =
    Oidc.For_testing.client_of_get (fun _ -> raise Eio.Time.Timeout)
  in
  Alcotest.(check bool)
    "timed out fetch fails without raising" true
    (Result.is_error (client.fetch_jwks ~uri:"https://example.test/keys"))

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

let test_oidc_rejects_invalid_endpoint uri () =
  let calls = ref 0 in
  let client =
    Oidc.For_testing.client_of_get (fun _ ->
        Int.incr calls;
        Error ())
  in
  Alcotest.(check bool)
    "controlled discovery failure" true
    (Result.is_error (client.discover ~issuer:uri));
  Alcotest.(check bool)
    "controlled keys failure" true
    (Result.is_error (client.fetch_jwks ~uri));
  Alcotest.(check int) "no connection attempted" 0 !calls;
  let document_calls = ref 0 in
  let issuer = "https://issuer.example" in
  let client =
    Oidc.For_testing.client_of_get (fun _ ->
        Int.incr document_calls;
        Ok
          ( Yojson.Safe.to_string
              (`Assoc [ ("issuer", `String issuer); ("jwks_uri", `String uri) ]),
            Cohttp.Header.init () ))
  in
  let verify =
    Oidc.make ~issuer ~audience:"kcal-client"
      ~clock:(fun () -> time 1_000.)
      ~client ()
  in
  Alcotest.(check bool)
    "provider-controlled key URL rejected" true
    (Result.is_error (verify (signed_token ())));
  Alcotest.(check int)
    "only valid discovery endpoint contacted" 1 !document_calls

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
          Alcotest.test_case "missing HTTPS host" `Quick
            (test_oidc_rejects_invalid_endpoint "https:///keys");
          Alcotest.test_case "invalid HTTPS host" `Quick
            (test_oidc_rejects_invalid_endpoint "https://-invalid.example/keys");
          Alcotest.test_case "numeric HTTPS host" `Quick
            (test_oidc_rejects_invalid_endpoint "https://127.0.0.1:443/keys");
          Alcotest.test_case "unknown key id refetches keys" `Quick
            test_oidc_refetches_keys_for_an_unknown_key_id;
          Alcotest.test_case "stale keys while the provider is down" `Quick
            test_oidc_serves_stale_keys_while_the_provider_is_down;
          Alcotest.test_case "unreachable provider is not hammered" `Quick
            test_oidc_does_not_hammer_an_unreachable_provider;
          Alcotest.test_case "concurrent requests share one refresh" `Quick
            test_oidc_concurrent_requests_share_one_refresh;
          Alcotest.test_case "timeout is a transport failure" `Quick
            test_oidc_timeout_is_a_transport_failure;
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
