open Base
open Hegel

let key =
  Option.value_exn (Result.ok (Secret.Key.of_bytes (Bytes.make 32 '\001')))

let test_round_trip () =
  let ciphertext =
    Option.value_exn (Result.ok (Secret.encrypt ~key "sensitive token"))
  in
  Alcotest.(check string)
    "plaintext recovers" "sensitive token"
    (Option.value_exn (Result.ok (Secret.decrypt ~key ciphertext)))

let test_rejects_changed_ciphertext () =
  let ciphertext =
    Option.value_exn (Result.ok (Secret.encrypt ~key "sensitive token"))
  in
  let changed = Bytes.of_string ciphertext in
  Bytes.set changed (Bytes.length changed - 1) 'x';
  Alcotest.(check bool)
    "tampering is rejected" true
    (Result.is_error (Secret.decrypt ~key (Bytes.to_string changed)))

let test_rejects_wrong_key () =
  let ciphertext =
    Option.value_exn (Result.ok (Secret.encrypt ~key "sensitive token"))
  in
  Alcotest.(check bool)
    "wrong key is rejected" true
    (Result.is_error
       (Secret.decrypt
          ~key:
            (Option.value_exn
               (Result.ok (Secret.Key.of_bytes (Bytes.make 32 '\002'))))
          ciphertext))

let test_key_is_validated_and_immutable () =
  List.iter
    ~f:(fun length ->
      Alcotest.(check bool)
        "wrong length" true
        (Result.is_error (Secret.Key.of_bytes (Bytes.make length 'k'))))
    [ 0; 31; 33 ];
  let bytes = Bytes.make 32 'k' in
  let key = Option.value_exn (Result.ok (Secret.Key.of_bytes bytes)) in
  let encrypted = Option.value_exn (Result.ok (Secret.encrypt ~key "token")) in
  Bytes.fill bytes ~pos:0 ~len:32 'x';
  Alcotest.(check string)
    "caller mutation cannot change validated key" "token"
    (Option.value_exn (Result.ok (Secret.decrypt ~key encrypted)))

let%hegel_test any_plaintext_round_trips tc =
  let plaintext = draw tc (binary ()) in
  let ciphertext =
    Option.value_exn (Result.ok (Secret.encrypt ~key plaintext))
  in
  assert (
    String.equal plaintext
      (Option.value_exn (Result.ok (Secret.decrypt ~key ciphertext))))

let%hegel_test any_changed_byte_is_rejected tc =
  let plaintext = draw tc (binary ()) in
  let ciphertext =
    Option.value_exn (Result.ok (Secret.encrypt ~key plaintext))
  in
  let index =
    draw tc (integers ~min_value:0 ~max_value:(String.length ciphertext - 1) ())
  in
  let flip = draw tc (integers ~min_value:1 ~max_value:255 ()) in
  let changed = Bytes.of_string ciphertext in
  Bytes.set changed index
    (Char.of_int_exn (Char.to_int (Bytes.get changed index) lxor flip));
  assert (Result.is_error (Secret.decrypt ~key (Bytes.to_string changed)))

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
          Alcotest.test_case "any plaintext round trips" `Quick
            any_plaintext_round_trips;
          Alcotest.test_case "any changed byte is rejected" `Quick
            any_changed_byte_is_rejected;
        ] );
    ]
