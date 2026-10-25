let id =
  Alcotest.testable
    (fun formatter value ->
      Format.pp_print_string formatter (User_id.to_string value))
    User_id.equal

let test_round_trip () =
  let value = User_id.fresh () in
  Alcotest.check id "string round-trip" value
    (Option.get (User_id.of_string (User_id.to_string value)));
  Alcotest.check id "sexp round-trip" value
    (User_id.t_of_sexp (User_id.sexp_of_t value));
  Alcotest.(check int) "same ID compares equal" 0 (User_id.compare value value)

let test_order () =
  let first =
    Option.get (User_id.of_string "00000000-0000-4000-8000-000000000000")
  in
  let last =
    Option.get (User_id.of_string "ffffffff-ffff-4fff-bfff-ffffffffffff")
  in
  Alcotest.(check bool) "distinct IDs unequal" false (User_id.equal first last);
  Alcotest.(check bool)
    "lexical UUID order" true
    (User_id.compare first last < 0);
  Alcotest.(check bool) "reverse order" true (User_id.compare last first > 0)

let test_invalid_sexp () =
  List.iter
    (fun sexp ->
      let rejected =
        try
          ignore (User_id.t_of_sexp sexp);
          false
        with Sexplib0.Sexp_conv.Of_sexp_error _ -> true
      in
      Alcotest.(check bool) "sexp cannot bypass UUID validation" true rejected)
    [ Base.Sexp.Atom "not-a-uuid"; Base.Sexp.List [] ]

let () =
  Alcotest.run "id"
    [
      ( "UUID",
        [
          Alcotest.test_case "round-trips" `Quick test_round_trip;
          Alcotest.test_case "ordering" `Quick test_order;
          Alcotest.test_case "reject invalid sexps" `Quick test_invalid_sexp;
        ] );
    ]
