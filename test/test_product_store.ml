module Domain_error = Error
open Base
module Error = Domain_error

let parsed store text =
  Option.value_exn
    (Result.ok (Store_sqlite.compile_search_query store [ text ]))

let input : Product.input =
  {
    name = "Whey 100";
    basis = "40 g";
    calories_kcal = 150;
    protein_g = 31.;
    carbs_g = Some 2.;
    fat_g = Some 1.;
    source = "label";
  }

let save store user ?id value =
  Option.value_exn
    (Result.ok (Test_support.save_product_raw store ~user ~id value))

let search store user ?(limit = 100) query =
  Option.value_exn
    (Result.ok
       (Store_sqlite.search_products store ~user ~query:(parsed store query)
          ~limit))

let check_equal = Alcotest.check
let invalid = function Error (Error.Invalid_input _) -> true | _ -> false

let invalid_field = function
  | Error (Error.Invalid_field _) -> true
  | _ -> false

let hidden = function Error Error.Not_found -> true | _ -> false

let test_save_and_replace () =
  Test_support.with_store (fun store user ->
      let meal = Test_support.create_meal store user in
      let product = save store user input in
      check_equal Test_support.user_id "owner" user.id product.user_id;
      check_equal Alcotest.string "name" "Whey 100" product.name;
      check_equal Alcotest.string "basis" "40 g" product.basis;
      check_equal Alcotest.int "kcal" 150 product.calories_kcal;
      check_equal Test_support.exact_float "protein" 31. product.protein_g;
      check_equal
        Alcotest.(option Test_support.exact_float)
        "carbs" (Some 2.) product.carbs_g;
      check_equal
        Alcotest.(option Test_support.exact_float)
        "fat" (Some 1.) product.fat_g;
      check_equal Alcotest.string "source" "label" product.source;
      check_equal Test_support.timestamp "initial timestamps" product.created_at
        product.updated_at;
      check_equal
        Alcotest.(list Test_support.product)
        "full search record" [ product ] (search store user "whey");
      let other = save store user { input with name = "Other" } in
      let replacement =
        { input with calories_kcal = 155; carbs_g = None; fat_g = None }
      in
      let updated = save store user ~id:product.id replacement in
      check_equal Test_support.product_id "same id" product.id updated.id;
      check_equal Test_support.timestamp "same creation time" product.created_at
        updated.created_at;
      check_equal Alcotest.int "replacement kcal" 155 updated.calories_kcal;
      check_equal
        Alcotest.(option Test_support.exact_float)
        "cleared carbs" None updated.carbs_g;
      check_equal
        Alcotest.(option Test_support.exact_float)
        "cleared fat" None updated.fat_g;
      check_equal
        Alcotest.(list Test_support.product)
        "persisted replacement" [ updated ] (search store user "whey");
      check_equal
        Alcotest.(list Test_support.product)
        "other untouched" [ other ]
        (search store user "Other");
      check_equal Test_support.meal "meal untouched" meal
        (Option.value_exn
           (Result.ok (Store_sqlite.get_meal store ~user meal.id))))

let test_validation () =
  Test_support.with_store (fun store user ->
      let values =
        [
          { input with name = " \t" };
          { input with basis = "" };
          { input with source = "\n" };
          { input with calories_kcal = -1 };
          { input with protein_g = -1. };
          { input with carbs_g = Some (-1.) };
          { input with fat_g = Some (-1.) };
        ]
        @ List.concat_map
            ~f:(fun value ->
              [
                { input with protein_g = value };
                { input with carbs_g = Some value };
                { input with fat_g = Some value };
              ])
            [ Float.nan; Float.infinity; Float.neg_infinity ]
      in
      List.iter
        ~f:(fun value ->
          check_equal Alcotest.bool "domain invalid" true
            (invalid_field (Product.Create.of_raw value));
          check_equal Alcotest.bool "save invalid" true
            (invalid_field
               (Test_support.save_product_raw store ~user ~id:None value)))
        values;
      ignore
        (save store user
           {
             input with
             calories_kcal = 0;
             protein_g = 0.;
             carbs_g = Some 0.;
             fat_g = Some 0.;
           });
      List.iter
        ~f:(fun query ->
          check_equal Alcotest.bool "blank query rejected" true
            (invalid (Store_sqlite.compile_search_query store [ query ])))
        [ ""; " \n" ];
      List.iter
        ~f:(fun limit ->
          check_equal Alcotest.bool "limit rejected" true
            (invalid
               (Store_sqlite.search_products store ~user
                  ~query:(parsed store "whey") ~limit)))
        [ 0; 501 ])

let test_isolation () =
  Test_support.with_two_users (fun store alice bob ->
      let product = save store alice input in
      check_equal
        Alcotest.(list Test_support.product)
        "foreign search empty" [] (search store bob "whey");
      List.iter
        ~f:(fun id ->
          check_equal Alcotest.bool "unknown or foreign update hidden" true
            (hidden
               (Test_support.save_product_raw store ~user:bob ~id:(Some id)
                  input)))
        [ product.id; Product_id.fresh () ];
      check_equal
        Alcotest.(list Test_support.product)
        "no accidental insert" [] (search store bob "whey");
      check_equal
        Alcotest.(list Test_support.product)
        "owner unchanged" [ product ]
        (search store alice "whey"))

let test_literal_search_and_order () =
  Test_support.with_store (fun store user ->
      let a = save store user { input with name = "A whey" } in
      let b = save store user { input with name = "A whey" } in
      let z = save store user { input with name = "Z whey" } in
      let expected =
        List.sort [ a; b ] ~compare:(fun (a : Product.t) b ->
            Product_id.compare a.id b.id)
      in
      check_equal Alcotest.bool "same names distinct" false
        (Product_id.equal a.id b.id);
      check_equal
        Alcotest.(list Test_support.product)
        "stable name/id order" (expected @ [ z ]) (search store user "WHEY");
      check_equal
        Alcotest.(list Test_support.product)
        "limit after matching and ordering"
        [ List.hd_exn expected ]
        (search store user ~limit:1 "whey");
      List.iter
        ~f:(fun (name, basis, source, query) ->
          let p = save store user { input with name; basis; source } in
          check_equal
            Alcotest.(list Test_support.product)
            ("literal " ^ query) [ p ] (search store user query))
        [
          ("100% food", "40 g", "label", "100");
          ("under_score", "40 g", "label", "score");
          ("Baker's food", "40 g", "label", "Baker's");
          ("Polarbröd", "40 g", "label", "POLARBRÖD");
          ("Basis match", "one portion", "label", "PORTION");
          ("Source match", "40 g", "estimated strawberry", "STRAWBERRY");
        ];
      check_equal
        Alcotest.(list Test_support.product)
        "no matches" []
        (search store user "nonexistent"))

let test_storage_errors () =
  Test_support.with_exclusive_lock
    (fun store user -> save store user input)
    (fun store user product ->
      let sanitized = function
        | Error (Error.Storage_error "SQLite operation failed") -> true
        | _ -> false
      in
      check_equal Alcotest.bool "locked insert" true
        (sanitized (Test_support.save_product_raw store ~user ~id:None input));
      check_equal Alcotest.bool "locked update" true
        (sanitized
           (Test_support.save_product_raw store ~user ~id:(Some product.id)
              input));
      check_equal Alcotest.bool "locked search" true
        (sanitized
           (Store_sqlite.search_products store ~user
              ~query:(parsed store "whey") ~limit:100)))

let test_corrupt_product_is_not_repaired () =
  Test_support.with_store (fun store user ->
      let product = save store user input in
      Alcotest.(check bool)
        "inject Float.infinity" true
        (match
           Sqlite3.exec
             (Store_sqlite.For_testing.raw_database store)
             "UPDATE products SET protein_g = 1e309"
         with
        | Sqlite3.Rc.OK -> true
        | _ -> false);
      Alcotest.(check bool)
        "search rejects corrupt product" true
        (match
           Store_sqlite.search_products store ~user ~query:(parsed store "whey")
             ~limit:100
         with
        | Error (Error.Storage_decoding _) -> true
        | _ -> false);
      Alcotest.(check bool)
        "replacement rejects corrupt product" true
        (match
           Test_support.save_product_raw store ~user ~id:(Some product.id) input
         with
        | Error (Error.Storage_decoding _) -> true
        | _ -> false))

let () =
  Alcotest.run "product store"
    [
      ( "references",
        List.map
          ~f:(fun (name, test) -> Alcotest.test_case name `Quick test)
          [
            ("save and replace", test_save_and_replace);
            ( "corrupt product is not repaired",
              test_corrupt_product_is_not_repaired );
            ("validation", test_validation);
            ("isolation", test_isolation);
            ("literal search and order", test_literal_search_and_order);
            ("sanitized storage errors", test_storage_errors);
          ] );
    ]
