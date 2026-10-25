let check = Alcotest.check

let json_rows =
  Alcotest.list
    (Alcotest.testable (Yojson.Safe.pretty_print ?std:None) Yojson.Safe.equal)

let product name =
  Product.
    {
      name;
      basis = "portion";
      calories_kcal = 100;
      protein_g = 10.;
      carbs_g = None;
      fat_g = None;
      source = "label";
    }

let save db user name =
  Result.get_ok (Test_support.save_product_raw db ~user ~id:None (product name))

let meal db user description =
  Result.get_ok
    (Test_support.create_meal_raw db ~user
       Meal.
         {
           (Test_support.meal_input ()) with
           description;
           estimate_source = None;
           notes = None;
         })

let request db user tool key value extras =
  Mcp.handle ~service:(Service.make ~store:db)
    ~identity:Auth.{ user; scopes = [ "ledger:read" ] }
    (`Assoc
       [
         ("jsonrpc", `String "2.0");
         ("method", `String "tools/call");
         ( "params",
           `Assoc
             [
               ("name", `String tool);
               ("arguments", `Assoc ((key, value) :: extras));
             ] );
       ])

let rows key = function
  | Ok (`Assoc response) -> (
      match List.assoc_opt "result" response with
      | Some (`Assoc result) -> (
          match List.assoc_opt "structuredContent" result with
          | Some (`Assoc fields) -> (
              match List.assoc_opt key fields with
              | Some (`List rows) -> rows
              | _ -> Alcotest.fail "missing result rows")
          | _ -> Alcotest.fail "missing structured result")
      | _ -> Alcotest.fail "missing structured result")
  | _ -> Alcotest.fail "MCP request failed"

let alternatives xs = `List (List.map (fun s -> `String s) xs)

let test_arrays () =
  Test_support.with_store (fun db user ->
      ignore (meal db user "ris kyckling");
      ignore (meal db user "rice chicken");
      let search value =
        rows "meals" (request db user "query_meals" "search" value [])
      in
      check Alcotest.int "both languages and reversed words" 2
        (List.length (search (alternatives [ "kyckling ris"; "chicken rice" ])));
      check Alcotest.int "no translation" 1
        (List.length (search (`String "chicken rice")));
      check json_rows "singleton equivalent"
        (search (`String "chicken rice"))
        (search (alternatives [ "chicken rice" ]));
      check json_rows "deduplicated"
        (search (alternatives [ "kyckling ris"; "chicken rice" ]))
        (search
           (alternatives [ "kyckling ris"; "chicken rice"; "chicken rice" ]));
      ignore (save db user "Whey protein");
      check Alcotest.int "product array" 1
        (List.length
           (rows "products"
              (request db user "search_products" "query"
                 (alternatives [ "vassleprotein"; "whey protein" ])
                 [ ("limit", `Int 5) ])));
      check json_rows "product singleton equivalent"
        (rows "products"
           (request db user "search_products" "query" (`String "whey protein")
              []))
        (rows "products"
           (request db user "search_products" "query"
              (alternatives [ "whey protein" ])
              [])))

let test_unicode_separators () =
  Test_support.with_store (fun db user ->
      List.iter
        (fun text ->
          ignore (save db user text);
          ignore (meal db user text))
        [ "rice chicken"; "chicken"; "rice"; "BRÖD WHEY"; "brod whey" ];
      List.iter
        (fun (tool, key, result_key) ->
          let search value =
            rows result_key (request db user tool key value [])
          in
          let expected = search (`String "chicken rice") in
          check Alcotest.int
            (tool ^ " reversed and partial overlap fixtures")
            3 (List.length expected);
          List.iter
            (fun text ->
              check json_rows
                (tool ^ " string " ^ text)
                expected
                (search (`String text));
              check json_rows
                (tool ^ " array " ^ text)
                expected
                (search (alternatives [ text; "CHICKEN" ])))
            [
              "chicken—rice";
              "chicken rice";
              "chicken rice";
              "chicken–rice";
              "CHICKEN—rice chicken RICE";
            ];
          check json_rows
            (tool ^ " Swedish and English folding after normalization")
            (search (`String "bröd whey"))
            (search (alternatives [ "BRÖD—WHEY"; "bröd whey" ]));
          check Alcotest.int
            (tool ^ " diacritics remain distinct")
            1
            (List.length (search (`String "bröd")));
          check Alcotest.int
            (tool ^ " unaccented distinct")
            1
            (List.length (search (`String "brod"))))
        [
          ("search_products", "query", "products");
          ("query_meals", "search", "meals");
        ])

let test_ranking () =
  Test_support.with_two_users (fun db user other ->
      ignore (save db user "chicken padding padding padding padding");
      let strong = save db user "rice chicken" in
      ignore (save db other "rice chicken rice chicken");
      let result =
        rows "products"
          (request db user "search_products" "query" (`String "chicken rice")
             [ ("limit", `Int 1) ])
      in
      check json_rows "rank before limit and owner isolation"
        [ Mcp.product_json strong ]
        result)

let test_v3_backfill () =
  Test_support.with_database (fun db ->
      List.iter
        (fun (v, sql) ->
          if v <= 3 then (
            Result.get_ok (Migration.execute db sql);
            Result.get_ok (Migration.record_version db v)))
        Store_sqlite.migrations;
      let user =
        Result.get_ok
          (Store_sqlite.resolve_user db ~issuer:"issuer" ~subject:"alice")
      in
      ignore (save db user "rice chicken");
      ignore (meal db user "ris kyckling");
      let deleted = meal db user "ris kyckling" in
      Result.get_ok (Store_sqlite.delete_meal db ~user deleted.id);
      Result.get_ok (Migration.apply_all db);
      Result.get_ok (Migration.apply_all db);
      check Alcotest.int "product backfilled" 1
        (List.length
           (rows "products"
              (request db user "search_products" "query"
                 (`String "chicken rice") [])));
      check Alcotest.int "meal backfilled" 1
        (List.length
           (rows "meals"
              (request db user "query_meals" "search" (`String "kyckling ris")
                 []))))

let test_invalid_wire_forms () =
  Test_support.with_store (fun db user ->
      let invalid = function
        | Ok (`Assoc fields) -> List.mem_assoc "error" fields
        | _ -> false
      in
      List.iter
        (fun (tool, key) ->
          List.iter
            (fun value ->
              check Alcotest.bool "Invalid_input wire mapping" true
                (invalid (request db user tool key value [])))
            [
              `Null;
              `Int 3;
              `List [];
              alternatives [ "whey"; " " ];
              `List [ `String "whey"; `Int 2 ];
              `String "***;_%'";
              alternatives [ "whey"; "***" ];
            ])
        [ ("search_products", "query"); ("query_meals", "search") ])

let test_plain_text_and_swedish () =
  Test_support.with_store (fun db user ->
      ignore (save db user "BRÖD");
      ignore (save db user "brod");
      ignore (save db user "Baker's OR NOT whey");
      let search text =
        rows "products"
          (request db user "search_products" "query" (`String text) [])
      in
      check Alcotest.int "Swedish case folds" 1 (List.length (search "bröd"));
      check Alcotest.int "diacritics stay distinct" 1
        (List.length (search "brod"));
      check json_rows "no prefixes" [] (search "br");
      List.iter
        (fun text ->
          check Alcotest.int "operators remain plain text" 1
            (List.length (search text)))
        [
          "OR";
          "NOT";
          "Baker's";
          "\"whey\"*";
          "whey' OR 1=1; DROP TABLE products;--";
        ];
      check Alcotest.int "Unicode punctuation safe" 1
        (List.length (search "Baker’s"));
      check json_rows "Unicode symbols yield empty results" [] (search "🥣");
      check Alcotest.int "source still exists" 1 (List.length (search "bröd")))

let test_index_lifecycle () =
  Test_support.with_two_users (fun db user other ->
      let p = save db user "oldword" in
      let find_products text =
        rows "products"
          (request db user "search_products" "query" (`String text) [])
      in
      let changed =
        Result.get_ok
          (Test_support.save_product_raw db ~user ~id:(Some p.id)
             (product "newword"))
      in
      check json_rows "replacement removes old terms" []
        (find_products "oldword");
      check json_rows "replacement indexes new terms"
        [ Mcp.product_json changed ]
        (find_products "newword");
      Result.get_ok
        (Migration.execute db "DELETE FROM products WHERE name = 'newword'");
      check json_rows "physical product delete" [] (find_products "newword");
      let m = meal db user "oldmeal" in
      let find_meals text =
        rows "meals" (request db user "query_meals" "search" (`String text) [])
      in
      ignore
        (Result.get_ok
           (Test_support.update_meal_raw db ~user m.id
              Meal.{ empty_patch with description = Some "newmeal" }));
      check json_rows "meal update removes old terms" [] (find_meals "oldmeal");
      check Alcotest.int "meal update adds new terms" 1
        (List.length (find_meals "newmeal"));
      ignore (meal db other "newmeal");
      Result.get_ok (Store_sqlite.delete_meal db ~user m.id);
      check json_rows "soft delete and foreign hidden" [] (find_meals "newmeal");
      let n = meal db user "physicalmeal" in
      Result.get_ok
        (Migration.execute db
           ("DELETE FROM meals WHERE id = '" ^ Meal_id.to_string n.id ^ "'"));
      check json_rows "physical meal delete" [] (find_meals "physicalmeal");
      Result.get_ok
        (Migration.execute db
           "INSERT INTO products_fts(products_fts) VALUES ('integrity-check')");
      Result.get_ok
        (Migration.execute db
           "INSERT INTO meals_fts(meals_fts) VALUES ('integrity-check')"))

let test_meal_ranked_filters () =
  Test_support.with_two_users (fun db user other ->
      let create owner time description =
        let eaten_at = Result.get_ok (Time.parse_offset_datetime time) in
        Result.get_ok
          (Test_support.create_meal_raw db ~user:owner
             Meal.
               {
                 (Test_support.meal_input ~eaten_at ()) with
                 description;
                 estimate_source = None;
                 notes = None;
               })
      in
      ignore
        (create user "2026-07-01T00:00:00Z"
           "chicken lots of padding lots of padding");
      let strong = create user "2026-07-02T00:00:00Z" "rice chicken" in
      ignore (create user "2026-06-01T00:00:00Z" "rice chicken rice chicken");
      ignore (create other "2026-07-01T00:00:00Z" "rice chicken rice chicken");
      let deleted =
        create user "2026-07-01T00:00:00Z" "rice chicken rice chicken"
      in
      Result.get_ok (Store_sqlite.delete_meal db ~user deleted.id);
      let value = alternatives [ "chicken rice"; "chicken"; "chicken rice" ] in
      let result =
        rows "meals"
          (request db user "query_meals" "search" value
             [ ("from", `String "2026-07-01T00:00:00Z"); ("limit", `Int 1) ])
      in
      check json_rows
        "ranking after owner date deleted filters and deduplication"
        [ Mcp.meal_json strong ]
        result)

let test_meal_stable_ties () =
  Test_support.with_store (fun db user ->
      let eaten_at =
        Result.get_ok (Time.parse_offset_datetime "2026-07-01T00:00:00Z")
      in
      let create () =
        Result.get_ok
          (Test_support.create_meal_raw db ~user
             Meal.
               {
                 (Test_support.meal_input ~eaten_at ()) with
                 description = "chicken rice";
               })
      in
      let a = create () and b = create () in
      let expected =
        List.sort (fun (a : Meal.t) b -> Meal_id.compare a.id b.id) [ a; b ]
      in
      check json_rows "meal relevance ties use time then id"
        (List.map Mcp.meal_json expected)
        (rows "meals"
           (request db user "query_meals" "search" (`String "rice chicken") [])))

let () =
  Alcotest.run "ranked search"
    [
      ( "behavior",
        List.map
          (fun (n, t) -> Alcotest.test_case n `Quick t)
          [
            ("Unicode separators", test_unicode_separators);
            ("arrays", test_arrays);
            ("ranking", test_ranking);
            ("v3 backfill", test_v3_backfill);
            ("invalid wire forms", test_invalid_wire_forms);
            ("safe plain text and Swedish", test_plain_text_and_swedish);
            ("index lifecycle", test_index_lifecycle);
            ("meal ranked filters", test_meal_ranked_filters);
            ("stable meal ties", test_meal_stable_ties);
          ] );
    ]
