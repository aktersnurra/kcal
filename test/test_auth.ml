let time seconds = Option.get (Ptime.of_float_s seconds)

let claims ?(issuer = "https://issuer.example") ?(subject = "alice")
    ?(audience = [ "kcal-client" ]) ?(expires_at = time 2_000_000_000.) () =
  Oidc.{ issuer; subject; audience; expires_at }

let verifier results = Oidc.of_verified_claims (fun _ -> results)

let test_rejects_expired_token () =
  let store, _ = Test_support.store_with_user () in
  let auth =
    Auth.make ~store ~verifier:(verifier (Ok (claims ~expires_at:(time 0.) ())))
  in
  Alcotest.(check bool) "unauthorized" true
    (match Auth.authenticate_bearer auth "expired-test-token" with
    | Error Error.Unauthorized -> true
    | _ -> false)

let test_resolves_verified_identity_stably () =
  let store = Test_support.open_temporary_database () in
  Result.get_ok (Migration.apply_all store);
  let auth = Auth.make ~store ~verifier:(verifier (Ok (claims ()))) in
  let first = Result.get_ok (Auth.authenticate_bearer auth "valid-token") in
  let second = Result.get_ok (Auth.authenticate_bearer auth "valid-token") in
  Alcotest.(check string) "stable user" (User_id.to_string first.id)
    (User_id.to_string second.id)

let test_rejects_unverified_token () =
  let store, _ = Test_support.store_with_user () in
  let auth = Auth.make ~store ~verifier:(verifier (Error ())) in
  Alcotest.(check bool) "unauthorized" true
    (match Auth.authenticate_bearer auth "malformed-token" with
    | Error Error.Unauthorized -> true
    | _ -> false)

let test_rejects_wrong_issuer_or_audience () =
  let store, _ = Test_support.store_with_user () in
  let wrong_issuer =
    Auth.make ~store
      ~verifier:(verifier (Ok (claims ~issuer:"https://other.example" ())))
  in
  let wrong_audience =
    Auth.make ~store
      ~verifier:(verifier (Ok (claims ~audience:[ "other-client" ] ())))
  in
  let unauthorized auth =
    match Auth.authenticate_bearer auth "valid-signature-wrong-claims" with
    | Error Error.Unauthorized -> true
    | _ -> false
  in
  Alcotest.(check bool) "issuer" true (unauthorized wrong_issuer);
  Alcotest.(check bool) "audience" true (unauthorized wrong_audience)

let () =
  Alcotest.run "auth"
    [
      ( "authentication",
        [ Alcotest.test_case "expired token" `Quick test_rejects_expired_token;
          Alcotest.test_case "stable verified identity" `Quick test_resolves_verified_identity_stably;
          Alcotest.test_case "unverified token" `Quick test_rejects_unverified_token;
          Alcotest.test_case "wrong issuer or audience" `Quick
            test_rejects_wrong_issuer_or_audience ] );
    ]
