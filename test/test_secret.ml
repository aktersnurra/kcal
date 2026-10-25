let key = Result.get_ok (Secret.Key.of_bytes (Bytes.make 32 '\001'))

let test_round_trip () =
  let ciphertext = Result.get_ok (Secret.encrypt ~key "sensitive token") in
  Alcotest.(check string)
    "plaintext recovers" "sensitive token"
    (Result.get_ok (Secret.decrypt ~key ciphertext))

let test_rejects_changed_ciphertext () =
  let ciphertext = Result.get_ok (Secret.encrypt ~key "sensitive token") in
  let changed = Bytes.of_string ciphertext in
  Bytes.set changed (Bytes.length changed - 1) 'x';
  Alcotest.(check bool)
    "tampering is rejected" true
    (Result.is_error (Secret.decrypt ~key (Bytes.to_string changed)))

let test_rejects_wrong_key () =
  let ciphertext = Result.get_ok (Secret.encrypt ~key "sensitive token") in
  Alcotest.(check bool)
    "wrong key is rejected" true
    (Result.is_error
       (Secret.decrypt
          ~key:(Result.get_ok (Secret.Key.of_bytes (Bytes.make 32 '\002')))
          ciphertext))

let test_key_is_validated_and_immutable () =
  List.iter
    (fun length ->
      Alcotest.(check bool)
        "wrong length" true
        (Result.is_error (Secret.Key.of_bytes (Bytes.make length 'k'))))
    [ 0; 31; 33 ];
  let bytes = Bytes.make 32 'k' in
  let key = Result.get_ok (Secret.Key.of_bytes bytes) in
  let encrypted = Result.get_ok (Secret.encrypt ~key "token") in
  Bytes.fill bytes 0 32 'x';
  Alcotest.(check string)
    "caller mutation cannot change validated key" "token"
    (Result.get_ok (Secret.decrypt ~key encrypted))

let () =
  Alcotest.run "secret"
    [
      ( "AES-GCM",
        [
          Alcotest.test_case "key validation and immutability" `Quick
            test_key_is_validated_and_immutable;
          Alcotest.test_case "round trip" `Quick test_round_trip;
          Alcotest.test_case "changed ciphertext" `Quick
            test_rejects_changed_ciphertext;
          Alcotest.test_case "wrong key" `Quick test_rejects_wrong_key;
        ] );
    ]
