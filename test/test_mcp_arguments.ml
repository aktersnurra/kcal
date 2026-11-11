module Domain_error = Error
open Base
module Error = Domain_error

let parse name fields =
  Mcp_arguments.parse ~now:Ptime.epoch
    ~default_zone:(fun () -> Ok Time.utc)
    ~name
    ~compile_search:(fun _ -> Alcotest.fail "unexpected search compilation")
    (`Assoc fields)

let test_tools_and_published_schemas_agree () =
  let published =
    List.map Mcp_schema.tools ~f:(fun tool ->
        match Yojson.Safe.Util.member "name" tool with
        | `String name -> name
        | _ -> Alcotest.fail "a published tool has no name")
  in
  let sorted = List.sort ~compare:String.compare in
  Alcotest.(check (list string))
    "every tool is published and every published tool exists"
    (sorted (List.map Mcp_arguments.Tool.all ~f:Mcp_arguments.Tool.name))
    (sorted published);
  List.iter Mcp_arguments.Tool.all ~f:(fun tool ->
      Alcotest.(check bool)
        "name round-trips" true
        (Option.is_some
           (Mcp_arguments.Tool.of_name (Mcp_arguments.Tool.name tool))))

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

let item_id = Meal_item_id.to_string (Meal_item_id.fresh ())
let meal_id = Meal_id.to_string (Meal_id.fresh ())

let test_patches_tell_absent_null_and_value_apart () =
  (match
     parse "update_meal_item"
       [
         ("id", `String item_id);
         ("carbs_g", `Null);
         ("notes", `String "with bread");
       ]
   with
  | Ok (Mcp_arguments.Update_meal_item { patch; _ }) ->
      let patch = Meal_item.Patch.to_raw patch in
      Alcotest.(check bool)
        "null clears" true
        (match patch.carbs_g with Clear -> true | _ -> false);
      Alcotest.(check bool)
        "a value sets" true
        (match patch.notes with Set "with bread" -> true | _ -> false);
      Alcotest.(check bool)
        "absent keeps" true
        (match patch.fat_g with Keep -> true | _ -> false)
  | _ -> Alcotest.fail "expected an item update");
  match parse "update_meal" [ ("id", `String meal_id); ("label", `Null) ] with
  | Ok (Mcp_arguments.Update_meal (_, patch)) ->
      Alcotest.(check bool)
        "null clears the label" true
        (match (Meal.Patch.to_raw patch).label with
        | Clear -> true
        | _ -> false)
  | _ -> Alcotest.fail "expected a meal update"

let test_required_fields_cannot_be_cleared () =
  List.iter
    ~f:(fun (tool, id, name) ->
      Alcotest.(check string)
        name
        (name
       ^ " cannot be cleared; give a new value, or omit it to keep the current \
          one")
        (message tool [ ("id", `String id); (name, `Null) ]))
    [
      ("update_meal_item", item_id, "description");
      ("update_meal_item", item_id, "calories_kcal");
      ("update_meal_item", item_id, "protein_g");
      ("update_meal", meal_id, "eaten_at");
    ]

let () =
  Alcotest.run "Mcp_arguments"
    [
      ( "query",
        [
          Alcotest.test_case "tools and published schemas agree" `Quick
            test_tools_and_published_schemas_agree;
          Alcotest.test_case "validates bounds" `Quick
            test_query_bounds_are_validated;
          Alcotest.test_case "defaults and endpoints" `Quick
            test_query_defaults_and_endpoints;
        ] );
      ( "errors",
        [
          Alcotest.test_case "name the field and the fix" `Quick
            test_errors_name_the_field_and_the_fix;
          Alcotest.test_case "required fields cannot be cleared" `Quick
            test_required_fields_cannot_be_cleared;
        ] );
      ( "patches",
        [
          Alcotest.test_case "absent, null and value differ" `Quick
            test_patches_tell_absent_null_and_value_apart;
        ] );
    ]
