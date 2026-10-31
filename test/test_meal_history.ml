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

let items_of meal =
  match field "items" meal with
  | `List items -> items
  | _ -> Alcotest.fail "items is not a list"

let only_item meal =
  match items_of meal with
  | [ item ] -> item
  | _ -> Alcotest.fail "expected one item"

let soup =
  `Assoc
    [
      ("description", `String "Soup");
      ("calories_kcal", `Int 250);
      ("protein_g", `Int 12);
    ]

let test_idempotency_key () =
  with_service (fun service user ->
      let meal =
        [ ("items", `List [ soup ]); ("idempotency_key", `String "soup-1") ]
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

let test_a_meal_has_items () =
  with_service (fun service user ->
      let meal =
        payload service user "record_meal"
          [
            ("label", `String "lunch");
            ( "items",
              `List
                [
                  soup;
                  `Assoc
                    [
                      ("description", `String "Bread");
                      ("calories_kcal", `Int 150);
                      ("protein_g", `Int 5);
                      ("carbs_g", `Int 28);
                    ];
                ] );
          ]
      in
      Alcotest.check json "totals are the items' sums" (`Int 400)
        (field "calories_kcal" meal);
      Alcotest.check json "protein summed" (`Float 17.) (field "protein_g" meal);
      Alcotest.check json "a macro only one item recorded" (`Float 28.)
        (field "carbs_g" meal);
      Alcotest.check json "label" (`String "lunch") (field "label" meal);
      let id = field "id" meal in
      let added =
        payload service user "add_meal_item"
          [
            ("meal_id", id);
            ( "item",
              `Assoc
                [
                  ("description", `String "Banana");
                  ("calories_kcal", `Int 100);
                  ("protein_g", `Int 1);
                ] );
          ]
      in
      Alcotest.(check (list string))
        "added last"
        [ "Soup"; "Bread"; "Banana" ]
        (List.map (items_of added) ~f:(string_field "description"));
      let banana = List.last_exn (items_of added) in
      let corrected =
        payload service user "update_meal_item"
          [
            ("id", field "id" banana);
            ("calories_kcal", `Int 120);
            ("reason", `String "a large banana");
          ]
      in
      Alcotest.check json "totals follow the correction" (`Int 520)
        (field "calories_kcal" corrected);
      let removed =
        payload service user "delete_meal_item" [ ("id", field "id" banana) ]
      in
      Alcotest.check json "totals without the banana" (`Int 400)
        (field "calories_kcal" removed);
      let moved =
        payload service user "update_meal"
          [ ("id", id); ("eaten_at", `String "2026-07-01T12:30:00Z") ]
      in
      Alcotest.check json "the whole meal moves"
        (`String "2026-07-01T12:30:00Z") (field "eaten_at" moved);
      Alcotest.check json "items unchanged by a meal update" (`Int 2)
        (`Int (List.length (items_of moved)));
      let daily =
        payload service user "get_daily_series"
          [
            ("from_date", `String "2026-07-01");
            ("to_date", `String "2026-07-01");
          ]
      in
      (match field "days" daily with
      | `List [ day ] ->
          Alcotest.check json "one meal, not two" (`Int 1)
            (field "meal_count" day)
      | _ -> Alcotest.fail "expected one day");
      let single =
        payload service user "record_meal" [ ("items", `List [ soup ]) ]
      in
      Alcotest.(check bool)
        "a meal keeps at least one item" true
        (String.is_substring
           (error_text service user "delete_meal_item"
              [ ("id", field "id" (only_item single)) ])
           ~substring:"at least one item");
      Alcotest.(check bool)
        "a meal without items is rejected" true
        (String.is_substring
           (error_text service user "record_meal" [ ("items", `List []) ])
           ~substring:"nonempty"))

let test_update_keeps_revisions () =
  with_service (fun service user ->
      let meal =
        payload service user "record_meal"
          [
            ( "items",
              `List
                [
                  `Assoc
                    [
                      ("description", `String "Pasta");
                      ("calories_kcal", `Int 500);
                      ("protein_g", `Int 20);
                      ("estimate_source", `String "photo estimate");
                    ];
                ] );
          ]
      in
      let item_id = field "id" (only_item meal) in
      ignore
        (payload service user "update_meal_item"
           [
             ("id", item_id);
             ("calories_kcal", `Int 650);
             ("reason", `String "portion was 300 g, not 200 g");
           ]);
      ignore
        (payload service user "update_meal_item"
           [ ("id", item_id); ("notes", `String "dinner") ]);
      let fetched =
        only_item (payload service user "get_meal" [ ("id", field "id" meal) ])
      in
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

let from_product product quantity =
  `Assoc [ ("product_id", field "id" product); ("quantity", `Int quantity) ]

let test_logging_from_a_product () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      Alcotest.check json "first version" (`Int 1) (field "version" product);
      let meal =
        payload service user "record_meal"
          [ ("items", `List [ from_product product 45; soup ]) ]
      in
      let item = List.hd_exn (items_of meal) in
      Alcotest.check json "kcal scaled 45/40 and rounded" (`Int 180)
        (field "calories_kcal" item);
      Alcotest.check json "protein scaled" (`Float 33.75)
        (field "protein_g" item);
      Alcotest.check json "description defaults to the name"
        (`String "Protein shake") (field "description" item);
      Alcotest.check json "linked to version 1"
        (`Assoc
           [
             ("product_id", field "id" product);
             ("product_version", `Int 1);
             ("quantity", `Float 45.);
             ("unit", `String "g");
           ])
        (field "product" item);
      Alcotest.check json "mixed with an estimated item" (`Int 430)
        (field "calories_kcal" meal);
      Alcotest.(check bool)
        "nutrition with a product is rejected" true
        (String.is_substring
           (error_text service user "record_meal"
              [
                ( "items",
                  `List
                    [
                      `Assoc
                        [
                          ("product_id", field "id" product);
                          ("quantity", `Int 45);
                          ("calories_kcal", `Int 1);
                        ];
                    ] );
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
              [ ("items", `List [ from_product unscalable 1 ]) ])
           ~substring:"basis_quantity");
      let edited =
        payload service user "update_meal_item"
          [ ("id", field "id" item); ("calories_kcal", `Int 200) ]
      in
      Alcotest.check json "a manual nutrition edit detaches the product" `Null
        (field "product" (List.hd_exn (items_of edited))))

let test_correcting_meals_from_a_product () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      let id = field "id" product in
      let meal =
        payload service user "record_meal"
          [ ("items", `List [ from_product product 40 ]) ]
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
      let get () =
        payload service user "get_meal" [ ("id", field "id" meal) ]
      in
      Alcotest.check json "saving never rewrites meals" (`Int 160)
        (field "calories_kcal" (get ()));
      let reason = ("reason", `String "label misread: 180 kcal per 40 g") in
      let preview =
        payload service user "correct_meals_from_product"
          [ ("product_id", id); reason ]
      in
      Alcotest.check json "dry run by default" (`Bool false)
        (field "applied" preview);
      (match field "items" preview with
      | `List [ planned ] ->
          Alcotest.check json "meal named" (field "id" meal)
            (field "meal_id" planned);
          Alcotest.check json "before" (`Int 160)
            (field "calories_kcal_before" planned);
          Alcotest.check json "after" (`Int 180)
            (field "calories_kcal_after" planned)
      | _ -> Alcotest.fail "expected one planned item");
      let applied =
        payload service user "correct_meals_from_product"
          [ ("product_id", id); reason; ("dry_run", `Bool false) ]
      in
      Alcotest.check json "applied" (`Bool true) (field "applied" applied);
      let item = only_item (get ()) in
      Alcotest.check json "rescaled" (`Int 180) (field "calories_kcal" item);
      Alcotest.check json "now on version 2" (`Int 2)
        (field "product_version" (field "product" item));
      (match field "revisions" item with
      | `List [ revision ] ->
          Alcotest.(check string)
            "reason recorded" "label misread: 180 kcal per 40 g"
            (string_field "reason" revision);
          Alcotest.check json "old link kept" (`Int 1)
            (field "product_version"
               (field "product" (field "previous" revision)))
      | _ -> Alcotest.fail "expected one revision");
      Alcotest.check json "nothing left to correct" (`List [])
        (field "items"
           (payload service user "correct_meals_from_product"
              [ ("product_id", id); reason; ("dry_run", `Bool false) ])))

let test_idempotency_replays_only_the_same_request () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      let request =
        [
          ("items", `List [ from_product product 40 ]);
          ("idempotency_key", `String "shake-1");
        ]
      in
      let first = payload service user "record_meal" request in
      Alcotest.check json "first call records" (`Bool false)
        (field "replayed" first);
      (* The product loses its structured basis, so resolving the items again
         would fail; a retry must still replay. *)
      ignore
        (payload service user "save_product"
           (("id", field "id" product)
           :: List.filter shake ~f:(fun (name, _) ->
               not (String.is_prefix name ~prefix:"basis_"))));
      let retry = payload service user "record_meal" (List.rev request) in
      Alcotest.check json "a retry replays" (`Bool true)
        (field "replayed" retry);
      Alcotest.check json "the same meal" (field "id" first) (field "id" retry);
      Alcotest.(check bool)
        "the same key with other content is rejected" true
        (String.is_substring
           (error_text service user "record_meal"
              [
                ("items", `List [ soup ]); ("idempotency_key", `String "shake-1");
              ])
           ~substring:"already used for a different meal"))

let test_updates_must_change_something () =
  with_service (fun service user ->
      let meal =
        payload service user "record_meal" [ ("items", `List [ soup ]) ]
      in
      let item_id = field "id" (only_item meal) in
      Alcotest.(check bool)
        "an empty item update is rejected" true
        (String.is_substring
           (error_text service user "update_meal_item"
              [ ("id", item_id); ("reason", `String "no change") ])
           ~substring:"give at least one field to change");
      Alcotest.(check bool)
        "an empty meal update is rejected" true
        (String.is_substring
           (error_text service user "update_meal" [ ("id", field "id" meal) ])
           ~substring:"give at least one field to change");
      Alcotest.check json "no revision was written" (`List [])
        (field "revisions"
           (only_item
              (payload service user "get_meal" [ ("id", field "id" meal) ]))))

let test_quantity_rescales_a_product_item () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      let meal =
        payload service user "record_meal"
          [ ("items", `List [ from_product product 45; soup ]) ]
      in
      let shake_item = List.hd_exn (items_of meal) in
      let doubled =
        List.hd_exn
          (items_of
             (payload service user "update_meal_item"
                [
                  ("id", field "id" shake_item);
                  ("quantity", `Int 90);
                  ("reason", `String "two portions");
                ]))
      in
      Alcotest.check json "kcal doubled" (`Int 360)
        (field "calories_kcal" doubled);
      Alcotest.check json "still linked, new quantity" (`Float 90.)
        (field "quantity" (field "product" doubled));
      Alcotest.(check bool)
        "quantity with nutrition is rejected" true
        (String.is_substring
           (error_text service user "update_meal_item"
              [
                ("id", field "id" shake_item);
                ("quantity", `Int 10);
                ("calories_kcal", `Int 1);
              ])
           ~substring:"not both");
      Alcotest.(check bool)
        "quantity on an estimated item is rejected" true
        (String.is_substring
           (error_text service user "update_meal_item"
              [
                ("id", field "id" (List.last_exn (items_of meal)));
                ("quantity", `Int 2);
              ])
           ~substring:"only applies to an item logged from a product");
      Alcotest.(check bool)
        "an unknown product is named" true
        (String.is_substring
           (error_text service user "record_meal"
              [
                ( "items",
                  `List
                    [
                      `Assoc
                        [
                          ( "product_id",
                            `String "12345678-1234-4234-8234-123456789abc" );
                          ("quantity", `Int 1);
                        ];
                    ] );
              ])
           ~substring:
             "product_id 12345678-1234-4234-8234-123456789abc was not found"))

let test_correction_skips_a_changed_unit () =
  with_service (fun service user ->
      let product = payload service user "save_product" shake in
      let id = field "id" product in
      let meal =
        payload service user "record_meal"
          [ ("items", `List [ from_product product 45 ]) ]
      in
      ignore
        (payload service user "save_product"
           (("id", id)
           :: List.map shake ~f:(function
             | "basis_quantity", _ -> ("basis_quantity", `Int 1)
             | "basis_unit", _ -> ("basis_unit", `String "scoop")
             | other -> other)));
      let applied =
        payload service user "correct_meals_from_product"
          [
            ("product_id", id);
            ("reason", `String "now per scoop");
            ("dry_run", `Bool false);
          ]
      in
      Alcotest.check json "nothing rescaled" (`List []) (field "items" applied);
      (match field "skipped" applied with
      | `List [ skipped ] ->
          Alcotest.(check bool)
            "the unit change is explained" true
            (String.is_substring
               (string_field "reason" skipped)
               ~substring:"logged in g, but the product is now per scoop")
      | _ -> Alcotest.fail "expected one skipped item");
      Alcotest.check json "the item keeps its values" (`Int 180)
        (field "calories_kcal"
           (only_item
              (payload service user "get_meal" [ ("id", field "id" meal) ]))))

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
           [ ("items", `List [ from_product product 40 ]) ]);
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
          Alcotest.test_case "a meal has items" `Quick test_a_meal_has_items;
          Alcotest.test_case "idempotency replays only the same request" `Quick
            test_idempotency_replays_only_the_same_request;
          Alcotest.test_case "updates must change something" `Quick
            test_updates_must_change_something;
          Alcotest.test_case "quantity rescales a product item" `Quick
            test_quantity_rescales_a_product_item;
          Alcotest.test_case "correction skips a changed unit" `Quick
            test_correction_skips_a_changed_unit;
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
