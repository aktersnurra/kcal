open Base

let identity user = Auth.{ user; scopes = [ "ledger:read"; "ledger:write" ] }

let call service user name arguments =
  match
    Mcp.handle ~service ~identity:(identity user)
      (`Assoc
         [
           ("jsonrpc", `String "2.0");
           ("method", `String "tools/call");
           ( "params",
             `Assoc [ ("name", `String name); ("arguments", `Assoc arguments) ]
           );
         ])
  with
  | Ok (`Assoc fields) -> (
      match List.Assoc.find fields ~equal:String.equal "result" with
      | Some (`Assoc result) -> result
      | _ -> Alcotest.fail (name ^ ": no result"))
  | _ -> Alcotest.fail (name ^ ": forbidden or malformed")

let field name = function
  | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal name with
      | Some value -> value
      | None -> Alcotest.fail ("missing " ^ name))
  | _ -> Alcotest.fail ("not an object looking for " ^ name)

let payload service user name arguments =
  let result = call service user name arguments in
  match List.Assoc.find result ~equal:String.equal "isError" with
  | Some (`Bool true) ->
      Alcotest.fail (name ^ " failed: " ^ Yojson.Safe.to_string (`Assoc result))
  | _ -> field "structuredContent" (`Assoc result)

let error_text service user name arguments =
  let result = `Assoc (call service user name arguments) in
  match (field "isError" result, field "content" result) with
  | `Bool true, `List [ content ] ->
      Yojson.Safe.Util.to_string (field "text" content)
  | _ -> Alcotest.fail (name ^ ": expected a tool error")

let json =
  Alcotest.testable (Yojson.Safe.pretty_print ?std:None) Yojson.Safe.equal

let string_field name value = Yojson.Safe.Util.to_string (field name value)

let shake =
  [
    ("name", `String "Protein shake");
    ("basis", `String "40 g");
    ("basis_quantity", `Int 40);
    ("basis_unit", `String "g");
    ("calories_kcal", `Int 160);
    ("protein_g", `Int 30);
    ("source", `String "label");
  ]

let with_service f =
  Test_support.with_store (fun store user -> f (Service.make ~store) user)

let test_idempotency_key () =
  with_service (fun service user ->
      let meal =
        [
          ("description", `String "Soup");
          ("calories_kcal", `Int 250);
          ("protein_g", `Int 12);
          ("idempotency_key", `String "soup-1");
        ]
      in
      let first = payload service user "record_meal" meal in
      let retry = payload service user "record_meal" meal in
      Alcotest.check json "retry returns the same meal" (field "id" first)
        (field "id" retry);
      let meals = payload service user "query_meals" [] in
      Alcotest.(check int)
        "one meal stored" 1
        (List.length (Yojson.Safe.Util.to_list (field "meals" meals)));
      ignore (payload service user "delete_meal" [ ("id", field "id" first) ]);
      Alcotest.(check bool)
        "a deleted meal's key is not reused" true
        (String.is_substring
           (error_text service user "record_meal" meal)
           ~substring:"deleted"))

let test_update_keeps_revisions () =
  with_service (fun service user ->
      let meal =
        payload service user "record_meal"
          [
            ("description", `String "Pasta");
            ("calories_kcal", `Int 500);
            ("protein_g", `Int 20);
            ("estimate_source", `String "photo estimate");
          ]
      in
      let id = field "id" meal in
      ignore
        (payload service user "update_meal"
           [
             ("id", id);
             ("calories_kcal", `Int 650);
             ("reason", `String "portion was 300 g, not 200 g");
           ]);
      ignore
        (payload service user "update_meal"
           [ ("id", id); ("notes", `String "dinner") ]);
      let fetched = payload service user "get_meal" [ ("id", id) ] in
      Alcotest.check json "current value" (`Int 650)
        (field "calories_kcal" fetched);
      match field "revisions" fetched with
      | `List [ first; second ] ->
          Alcotest.check json "first revision" (`Int 1) (field "revision" first);
          Alcotest.check json "reason kept"
            (`String "portion was 300 g, not 200 g") (field "reason" first);
          Alcotest.check json "original estimate kept" (`Int 500)
            (field "calories_kcal" (field "previous" first));
          Alcotest.check json "original source kept" (`String "photo estimate")
            (field "estimate_source" (field "previous" first));
          Alcotest.check json "reason optional" `Null (field "reason" second);
          Alcotest.check json "second snapshot has the corrected value"
            (`Int 650)
            (field "calories_kcal" (field "previous" second))
      | _ -> Alcotest.fail "expected two revisions")

let test_logging_from_a_product () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      Alcotest.check json "first version" (`Int 1) (field "version" product);
      let meal =
        payload service user "record_meal"
          [ ("product_id", field "id" product); ("quantity", `Int 45) ]
      in
      Alcotest.check json "kcal scaled 45/40 and rounded" (`Int 180)
        (field "calories_kcal" meal);
      Alcotest.check json "protein scaled" (`Float 33.75)
        (field "protein_g" meal);
      Alcotest.check json "description defaults to the name"
        (`String "Protein shake") (field "description" meal);
      Alcotest.check json "linked to version 1"
        (`Assoc
           [
             ("product_id", field "id" product);
             ("product_version", `Int 1);
             ("quantity", `Float 45.);
           ])
        (field "product" meal);
      Alcotest.(check bool)
        "nutrition with a product is rejected" true
        (String.is_substring
           (error_text service user "record_meal"
              [
                ("product_id", field "id" product);
                ("quantity", `Int 45);
                ("calories_kcal", `Int 1);
              ])
           ~substring:"omit calories_kcal");
      let unscalable =
        payload service user "save_product"
          [
            ("name", `String "Lasagne");
            ("basis", `String "one portion");
            ("calories_kcal", `Int 700);
            ("protein_g", `Int 35);
            ("source", `String "estimate");
          ]
      in
      Alcotest.(check bool)
        "a product without a structured basis explains itself" true
        (String.is_substring
           (error_text service user "record_meal"
              [ ("product_id", field "id" unscalable); ("quantity", `Int 1) ])
           ~substring:"basis_quantity");
      let edited =
        payload service user "update_meal"
          [ ("id", field "id" meal); ("calories_kcal", `Int 200) ]
      in
      Alcotest.check json "a manual nutrition edit detaches the product" `Null
        (field "product" edited))

let test_correcting_meals_from_a_product () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      let id = field "id" product in
      let meal =
        payload service user "record_meal"
          [ ("product_id", id); ("quantity", `Int 40) ]
      in
      let replaced =
        payload service user "save_product"
          (("id", id)
          :: List.map shake ~f:(function
            | "calories_kcal", _ -> ("calories_kcal", `Int 180)
            | other -> other))
      in
      Alcotest.check json "replacement bumps the version" (`Int 2)
        (field "version" replaced);
      Alcotest.check json "saving never rewrites meals" (`Int 160)
        (field "calories_kcal"
           (payload service user "get_meal" [ ("id", field "id" meal) ]));
      let reason = ("reason", `String "label misread: 180 kcal per 40 g") in
      let preview =
        payload service user "correct_meals_from_product"
          [ ("product_id", id); reason ]
      in
      Alcotest.check json "dry run by default" (`Bool false)
        (field "applied" preview);
      (match field "meals" preview with
      | `List [ planned ] ->
          Alcotest.check json "before" (`Int 160)
            (field "calories_kcal_before" planned);
          Alcotest.check json "after" (`Int 180)
            (field "calories_kcal_after" planned)
      | _ -> Alcotest.fail "expected one planned meal");
      let applied =
        payload service user "correct_meals_from_product"
          [ ("product_id", id); reason; ("dry_run", `Bool false) ]
      in
      Alcotest.check json "applied" (`Bool true) (field "applied" applied);
      let fetched =
        payload service user "get_meal" [ ("id", field "id" meal) ]
      in
      Alcotest.check json "rescaled" (`Int 180) (field "calories_kcal" fetched);
      Alcotest.check json "now on version 2" (`Int 2)
        (field "product_version" (field "product" fetched));
      (match field "revisions" fetched with
      | `List [ revision ] ->
          Alcotest.(check string)
            "reason recorded" "label misread: 180 kcal per 40 g"
            (string_field "reason" revision);
          Alcotest.check json "old link kept" (`Int 1)
            (field "product_version"
               (field "product" (field "previous" revision)))
      | _ -> Alcotest.fail "expected one revision");
      Alcotest.check json "nothing left to correct" (`List [])
        (field "meals"
           (payload service user "correct_meals_from_product"
              [ ("product_id", id); reason; ("dry_run", `Bool false) ])))

let test_goals_round_trip_and_context () =
  with_service (fun service user ->
      let goals =
        [
          ("timezone", `String "Europe/Stockholm");
          ("weekday_calorie_target_kcal", `Int 1750);
          ("weekend_calorie_target_kcal", `Int 1950);
          ("eating_window_start", `String "12:00");
          ("eating_window_end", `String "20:00");
          ("target_weight_kg", `Int 70);
        ]
      in
      let saved = payload service user "set_goals" goals in
      Alcotest.check json "16:8" (`Float 16.) (field "fasting_hours" saved);
      Alcotest.check json "no protein target" `Null
        (field "protein_target_g" saved);
      let context = payload service user "get_context" [] in
      Alcotest.check json "goals in context" saved (field "goals" context);
      Alcotest.check json "context in the goals timezone"
        (`String "Europe/Stockholm") (field "timezone" context);
      Alcotest.(check bool)
        "eating window reported" true
        (match field "open" (field "eating_window" context) with
        | `Bool _ -> true
        | _ -> false);
      Alcotest.(check bool)
        "half a window is rejected" true
        (String.is_substring
           (error_text service user "set_goals"
              (List.filter goals ~f:(fun (name, _) ->
                   not (String.equal name "eating_window_end"))))
           ~substring:"together");
      let cleared =
        payload service user "set_goals"
          (List.filter goals ~f:(fun (name, _) ->
               not (String.is_prefix name ~prefix:"eating_window")))
      in
      Alcotest.check json "replacement clears omitted goals" `Null
        (field "eating_window_start" cleared))

let test_recent_products_in_context () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      ignore
        (payload service user "record_meal"
           [ ("product_id", field "id" product); ("quantity", `Int 40) ]);
      match field "recent_products" (payload service user "get_context" []) with
      | `List [ recent ] ->
          Alcotest.check json "the shake" (field "id" product)
            (field "id" recent)
      | _ -> Alcotest.fail "expected one recent product")

let () =
  Alcotest.run "meal history"
    [
      ( "history",
        [
          Alcotest.test_case "idempotency key" `Quick test_idempotency_key;
          Alcotest.test_case "update keeps revisions" `Quick
            test_update_keeps_revisions;
          Alcotest.test_case "logging from a product" `Quick
            test_logging_from_a_product;
          Alcotest.test_case "correcting meals from a product" `Quick
            test_correcting_meals_from_a_product;
          Alcotest.test_case "goals round trip and context" `Quick
            test_goals_round_trip_and_context;
          Alcotest.test_case "recent products in context" `Quick
            test_recent_products_in_context;
        ] );
    ]
