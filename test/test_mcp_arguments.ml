module Domain_error = Error
open Base
module Error = Domain_error

let parse name fields =
  Mcp_arguments.parse ~now:Ptime.epoch
    ~default_zone:(fun () -> Ok Time.utc)
    ~name
    ~compile_search:(fun _ -> Alcotest.fail "unexpected search compilation")
    (`Assoc fields)

let test_query_bounds_are_validated () =
  List.iter [ 0; -1; 501; Int.max_value ] ~f:(fun limit ->
      Alcotest.(check bool)
        "query limit rejected" true
        (Result.is_error (parse "query_weights" [ ("limit", `Int limit) ])));
  Alcotest.(check bool)
    "reversed times rejected" true
    (Result.is_error
       (parse "query_weights"
          [
            ("from", `String "2026-10-25T12:00:00Z");
            ("to", `String "2026-10-25T11:00:00Z");
          ]))

let test_query_defaults_and_endpoints () =
  List.iter
    [ []; [ ("limit", `Int 1) ]; [ ("limit", `Int 500) ] ]
    ~f:(fun fields ->
      match parse "query_weights" fields with
      | Ok (Mcp_arguments.Query_weights request) ->
          Alcotest.(check int)
            "validated limit"
            (match fields with [ (_, `Int limit) ] -> limit | _ -> 100)
            request.limit
      | _ -> Alcotest.fail "expected a validated weight query")

let message name fields =
  match parse name fields with
  | Error error -> Error.to_string error
  | Ok _ -> Alcotest.fail (name ^ ": expected an argument error")

let item fields = [ ("items", `List [ `Assoc fields ]) ]

let soup =
  [
    ("description", `String "Soup");
    ("calories_kcal", `Int 250);
    ("protein_g", `Int 12);
  ]

let test_errors_name_the_field_and_the_fix () =
  List.iter
    ~f:(fun (label, name, fields, expected) ->
      Alcotest.(check string) label expected (message name fields))
    [
      ( "unknown field suggests the intended one",
        "record_meal",
        item
          [
            ("description", `String "Soup");
            ("kcal", `Int 250);
            ("protein_g", `Int 12);
          ],
        "unknown field \"kcal\" (did you mean \"calories_kcal\"?); allowed \
         fields: description, calories_kcal, protein_g, carbs_g, fat_g, \
         confidence, estimate_source, notes" );
      ( "missing required field",
        "record_meal",
        item [ ("description", `String "Soup"); ("calories_kcal", `Int 250) ],
        "protein_g is required (a number)" );
      ( "wrong type",
        "record_meal",
        item
          (List.Assoc.add soup ~equal:String.equal "protein_g" (`String "lots")),
        "protein_g must be a number" );
      ( "malformed timestamp",
        "record_meal",
        ("eaten_at", `String "yesterday") :: item soup,
        "eaten_at must be an RFC 3339 timestamp with an offset, e.g. \
         2026-09-22T12:30:00Z or 2026-09-22T14:30:00+02:00" );
      ( "malformed id",
        "get_meal",
        [ ("id", `String "abc") ],
        "id \"abc\" is not a valid id; use one returned by an earlier call" );
      ( "limit out of range",
        "query_weights",
        [ ("limit", `Int 0) ],
        "limit must be between 1 and 500" );
      ( "domain validation is explained",
        "record_meal",
        item (List.Assoc.add soup ~equal:String.equal "protein_g" (`Int (-1))),
        "protein_g must be nonnegative" );
      ("unknown tool", "record_lunch", [], "unknown tool record_lunch");
    ]

let () =
  Alcotest.run "Mcp_arguments"
    [
      ( "query",
        [
          Alcotest.test_case "validates bounds" `Quick
            test_query_bounds_are_validated;
          Alcotest.test_case "defaults and endpoints" `Quick
            test_query_defaults_and_endpoints;
        ] );
      ( "errors",
        [
          Alcotest.test_case "name the field and the fix" `Quick
            test_errors_name_the_field_and_the_fix;
        ] );
    ]
