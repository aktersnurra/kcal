let parsed store text = Result.get_ok (Search_query.of_strings store [ text ])

let test_user_resolution_is_stable () =
  Test_support.with_store (fun store first ->
      let second =
        Result.get_ok
          (Store_sqlite.resolve_user store ~issuer:"https://issuer.example"
             ~subject:"alice")
      in
      Alcotest.(check string)
        "same user"
        (User_id.to_string first.id)
        (User_id.to_string second.id))

let test_cannot_read_another_users_meal () =
  Test_support.with_two_users (fun store alice bob ->
      let meal = Test_support.create_meal store alice in
      Alcotest.(check bool)
        "hidden" true
        (match Store_sqlite.get_meal store ~user:bob meal.id with
        | Error Error.Not_found -> true
        | _ -> false))

let test_foreign_meal_operations_are_hidden () =
  Test_support.with_two_users (fun store alice bob ->
      let meal = Test_support.create_meal store alice in
      let hidden result =
        match result with Error Error.Not_found -> true | _ -> false
      in
      Alcotest.(check bool)
        "foreign get" true
        (hidden (Store_sqlite.get_meal store ~user:bob meal.id));
      Alcotest.(check bool)
        "foreign update" true
        (hidden
           (Store_sqlite.update_meal store ~user:bob meal.id Meal.empty_patch));
      Alcotest.(check bool)
        "foreign delete" true
        (hidden (Store_sqlite.delete_meal store ~user:bob meal.id));
      Alcotest.(check int)
        "foreign query" 0
        (List.length
           (Result.get_ok
              (Store_sqlite.query_meals store ~user:bob ~from:None ~to_:None
                 ~limit:100))))

let test_meal_crud_and_soft_delete () =
  Test_support.with_store (fun store user ->
      let meal = Test_support.create_meal store user in
      let patch =
        Meal.
          {
            empty_patch with
            description = Some "Stew";
            calories_kcal = Some 300;
          }
      in
      let updated =
        Result.get_ok (Store_sqlite.update_meal store ~user meal.id patch)
      in
      Alcotest.(check string) "description" "Stew" updated.description;
      Alcotest.(check int) "calories" 300 updated.calories_kcal;
      Result.get_ok (Store_sqlite.delete_meal store ~user meal.id);
      Result.get_ok (Store_sqlite.delete_meal store ~user meal.id);
      Alcotest.(check bool)
        "deleted hidden" true
        (match Store_sqlite.get_meal store ~user meal.id with
        | Error Error.Not_found -> true
        | _ -> false))

let test_deleted_meal_is_excluded_from_query () =
  Test_support.with_store (fun store user ->
      let meal = Test_support.create_meal store user in
      Result.get_ok (Store_sqlite.delete_meal store ~user meal.id);
      Alcotest.(check int)
        "visible meals" 0
        (List.length
           (Result.get_ok
              (Store_sqlite.query_meals store ~user ~from:None ~to_:None
                 ~limit:100))))

let test_partial_update_leaves_omitted_fields_unchanged () =
  Test_support.with_store (fun store user ->
      let meal = Test_support.create_meal store user in
      let updated =
        Result.get_ok
          (Store_sqlite.update_meal store ~user meal.id
             Meal.{ empty_patch with confidence = Some 0.5 })
      in
      Alcotest.(check (float 0.0001))
        "confidence updated" 0.5
        (Option.get updated.confidence);
      Alcotest.(check string)
        "description untouched" meal.description updated.description;
      Alcotest.(check int)
        "calories untouched" meal.calories_kcal updated.calories_kcal;
      Alcotest.(check (float 0.0001))
        "protein untouched" meal.protein_g updated.protein_g;
      Alcotest.(check (option (float 0.0001)))
        "carbs untouched" meal.carbs_g updated.carbs_g;
      Alcotest.(check (option (float 0.0001)))
        "fat untouched" meal.fat_g updated.fat_g;
      Alcotest.(check (option string))
        "estimate source untouched" meal.estimate_source updated.estimate_source;
      Alcotest.(check (option string))
        "notes untouched" meal.notes updated.notes;
      Alcotest.(check string)
        "eaten_at untouched"
        (Time.to_utc_string meal.eaten_at)
        (Time.to_utc_string updated.eaten_at))

let test_locked_meal_get_and_update_are_storage_errors () =
  Test_support.with_exclusive_lock
    (fun store user -> (Test_support.create_meal store user, Meal.empty_patch))
    (fun store user (meal, patch) ->
      let is_storage_error = function
        | Error (Error.Storage_error "SQLite operation failed") -> true
        | _ -> false
      in
      Alcotest.(check bool)
        "locked get is sanitized" true
        (is_storage_error (Store_sqlite.get_meal store ~user meal.id));
      Alcotest.(check bool)
        "locked update is sanitized" true
        (is_storage_error (Store_sqlite.update_meal store ~user meal.id patch)))

let test_meals_between_is_half_open_and_skips_deleted () =
  Test_support.with_store (fun store user ->
      let at value = Result.get_ok (Time.parse_offset_datetime value) in
      let create value =
        Result.get_ok
          (Store_sqlite.create_meal store ~user
             (Test_support.meal_input ~eaten_at:(at value) ()))
      in
      let first = create "2026-09-22T00:00:00Z" in
      let second = create "2026-09-22T20:00:00Z" in
      (* Just outside [start, stop) on either side; must not be returned. *)
      ignore (create "2026-09-23T00:00:00Z");
      ignore (create "2026-09-21T23:59:59Z");
      let start = at "2026-09-22T00:00:00Z"
      and stop = at "2026-09-23T00:00:00Z" in
      let ids () =
        List.map
          (fun (meal : Meal.t) -> Meal_id.to_string meal.id)
          (Result.get_ok (Store_sqlite.meals_between store ~user ~start ~stop))
      in
      Alcotest.(check (list string))
        "in range, oldest first"
        [ Meal_id.to_string first.id; Meal_id.to_string second.id ]
        (ids ());
      Result.get_ok (Store_sqlite.delete_meal store ~user second.id);
      Alcotest.(check (list string))
        "deleted meal excluded"
        [ Meal_id.to_string first.id ]
        (ids ());
      let empty_start = at "2026-09-25T00:00:00Z"
      and empty_stop = at "2026-09-26T00:00:00Z" in
      Alcotest.(check int)
        "empty range" 0
        (List.length
           (Result.get_ok
              (Store_sqlite.meals_between store ~user ~start:empty_start
                 ~stop:empty_stop))))

let test_rejects_invalid_limit () =
  Test_support.with_store (fun store user ->
      List.iter
        (fun limit ->
          Alcotest.(check bool)
            "invalid limit" true
            (match
               Store_sqlite.query_meals store ~user ~from:None ~to_:None ~limit
             with
            | Error (Error.Invalid_input _) -> true
            | _ -> false))
        [ 0; 501 ])

let test_search_matches_literal_text_and_scopes_results () =
  Test_support.with_two_users (fun store user foreign ->
      let create user description estimate_source notes =
        Result.get_ok
          (Store_sqlite.create_meal store ~user
             Meal.
               {
                 (Test_support.meal_input ()) with
                 description;
                 estimate_source;
                 notes;
               })
      in
      let description = create user "WHEY shake" None None in
      let source = create user "Shake" (Some "whey label") None in
      let notes = create user "Snack" None (Some "with Whey") in
      let bread = create user "Polarbröd" None None in
      let percent = create user "20% protein" None None in
      let underscore = create user "food_bar" None None in
      let apostrophe = create user "Baker's loaf" None None in
      ignore (create user "Unrelated" None None);
      ignore (create foreign "whey Polarbröd 20% food_bar Baker's" None None);
      let deleted =
        create user "whey Polarbröd 20% food_bar Baker's" None None
      in
      Result.get_ok (Store_sqlite.delete_meal store ~user deleted.id);
      List.iter
        (fun (search, expected) ->
          let meals =
            Result.get_ok
              (Store_sqlite.query_meals ~search:(parsed store search) store
                 ~user ~from:None ~to_:None ~limit:100)
          in
          let ids meals =
            List.map (fun (meal : Meal.t) -> Meal_id.to_string meal.id) meals
            |> List.sort String.compare
          in
          Alcotest.(check (list string)) search (ids expected) (ids meals))
        [
          ("whey", [ description; source; notes ]);
          ("POLARBRÖD", [ bread ]);
          ("20", [ percent ]);
          ("bar", [ underscore ]);
          ("Baker's", [ apostrophe ]);
          ("missing", []);
        ])

let test_search_composes_with_dates_order_and_limit () =
  Test_support.with_store (fun store user ->
      let service = Service.make ~store in
      let at value = Result.get_ok (Time.parse_offset_datetime value) in
      let create time description =
        Result.get_ok
          (Store_sqlite.create_meal store ~user
             Meal.
               {
                 (Test_support.meal_input ~eaten_at:(at time) ()) with
                 description;
               })
      in
      (* Insert out of chronological order to pin ordering, not insertion order. *)
      let last = create "2026-09-23T00:00:00Z" "whey last" in
      let unrelated = create "2026-09-21T00:00:00Z" "Soup" in
      let first = create "2026-09-22T00:00:00Z" "whey first" in
      let middle = create "2026-09-22T12:00:00Z" "whey middle" in
      let check label expected result =
        let ids = List.map (fun (meal : Meal.t) -> Meal_id.to_string meal.id) in
        Alcotest.(check (list string))
          label (ids expected)
          (ids (Result.get_ok result))
      in
      let query ?search ?(from = None) ?(to_ = None) limit =
        Service.query_meals
          ?search:(Option.map (parsed store) search)
          service ~user ~from ~to_ ~limit
      in
      check "filter before limit" [ first ] (query ~search:"whey" 1);
      check "oldest first" [ first; middle; last ] (query ~search:"whey" 100);
      check "inclusive bounds" [ first; middle ]
        (query ~search:"whey" ~from:(Some first.eaten_at)
           ~to_:(Some middle.eaten_at) 100);
      check "equal inclusive bounds" [ middle ]
        (query ~search:"whey" ~from:(Some middle.eaten_at)
           ~to_:(Some middle.eaten_at) 100);
      check "omitted search" [ unrelated; first; middle; last ] (query 100))

let test_search_rejects_blank_text_and_invalid_limits () =
  Test_support.with_store (fun store user ->
      List.iter
        (fun (search, limit) ->
          Alcotest.(check bool)
            "invalid search query" true
            (match
               match Search_query.of_strings store [ search ] with
               | Error e -> Error e
               | Ok search ->
                   Store_sqlite.query_meals ~search store ~user ~from:None
                     ~to_:None ~limit
             with
            | Error (Error.Invalid_input _) -> true
            | _ -> false))
        [ ("", 100); (" \t\r\n", 100); ("whey", 0); ("whey", 501) ])

let () =
  Alcotest.run "meal store"
    [
      ( "persistence",
        [
          Alcotest.test_case "literal search across fields is owner scoped"
            `Quick test_search_matches_literal_text_and_scopes_results;
          Alcotest.test_case "search composes with dates ordering and limit"
            `Quick test_search_composes_with_dates_order_and_limit;
          Alcotest.test_case "search rejects blank text and invalid limits"
            `Quick test_search_rejects_blank_text_and_invalid_limits;
          Alcotest.test_case "user resolution is stable" `Quick
            test_user_resolution_is_stable;
          Alcotest.test_case "users cannot read each other's meals" `Quick
            test_cannot_read_another_users_meal;
          Alcotest.test_case "foreign meal operations are hidden" `Quick
            test_foreign_meal_operations_are_hidden;
          Alcotest.test_case "meal CRUD and soft delete" `Quick
            test_meal_crud_and_soft_delete;
          Alcotest.test_case "partial update leaves omitted fields unchanged"
            `Quick test_partial_update_leaves_omitted_fields_unchanged;
          Alcotest.test_case "deleted meals are excluded from queries" `Quick
            test_deleted_meal_is_excluded_from_query;
          Alcotest.test_case "locked meal get and update are sanitized" `Quick
            test_locked_meal_get_and_update_are_storage_errors;
          Alcotest.test_case "invalid query limits are rejected" `Quick
            test_rejects_invalid_limit;
          Alcotest.test_case
            "meals between is half-open and skips deleted meals" `Quick
            test_meals_between_is_half_open_and_skips_deleted;
        ] );
    ]
