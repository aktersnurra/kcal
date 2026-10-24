let parsed text = Result.get_ok (Search_query.of_strings [text])
let input : Product.input =
  { name = "Whey 100"; basis = "40 g"; calories_kcal = 150; protein_g = 31.;
    carbs_g = Some 2.; fat_g = Some 1.; source = "label" }

let save store user ?id value = Result.get_ok (Store_sqlite.save_product store ~user ~id value)
let search store user ?(limit = 100) query =
  Result.get_ok (Store_sqlite.search_products store ~user ~query:(parsed query) ~limit)
let check_equal label expected actual = Alcotest.(check bool) label true (expected = actual)
let invalid = function Error (Error.Invalid_input _) -> true | _ -> false
let hidden = function Error Error.Not_found -> true | _ -> false

let test_save_and_replace () =
  let store, user = Test_support.store_with_user () in
  let meal = Test_support.create_meal store user in
  let product = save store user input in
  check_equal "owner" user.id product.user_id;
  check_equal "name" "Whey 100" product.name;
  check_equal "basis" "40 g" product.basis;
  check_equal "kcal" 150 product.calories_kcal;
  check_equal "protein" 31. product.protein_g;
  check_equal "carbs" (Some 2.) product.carbs_g;
  check_equal "fat" (Some 1.) product.fat_g;
  check_equal "source" "label" product.source;
  check_equal "initial timestamps" product.created_at product.updated_at;
  check_equal "full search record" [product] (search store user "whey");
  let other = save store user { input with name = "Other" } in
  let replacement = { input with calories_kcal = 155; carbs_g = None; fat_g = None } in
  let updated = save store user ~id:product.id replacement in
  check_equal "same id" product.id updated.id;
  check_equal "same creation time" product.created_at updated.created_at;
  check_equal "replacement kcal" 155 updated.calories_kcal;
  check_equal "cleared carbs" None updated.carbs_g;
  check_equal "cleared fat" None updated.fat_g;
  check_equal "persisted replacement" [updated] (search store user "whey");
  check_equal "other untouched" [other] (search store user "Other");
  check_equal "meal untouched" meal (Result.get_ok (Store_sqlite.get_meal store ~user meal.id))

let test_validation () =
  let store, user = Test_support.store_with_user () in
  let values = [ { input with name = " \t" }; { input with basis = "" };
    { input with source = "\n" }; { input with calories_kcal = -1 };
    { input with protein_g = -1. }; { input with carbs_g = Some (-1.) };
    { input with fat_g = Some (-1.) } ] @
    List.concat_map (fun value -> [ { input with protein_g = value };
      { input with carbs_g = Some value }; { input with fat_g = Some value } ])
      [nan; infinity; neg_infinity] in
  List.iter (fun value ->
    check_equal "domain invalid" true (invalid (Product.validate_input value));
    check_equal "save invalid" true (invalid (Store_sqlite.save_product store ~user ~id:None value))) values;
  ignore (save store user { input with calories_kcal = 0; protein_g = 0.; carbs_g = Some 0.; fat_g = Some 0. });
  List.iter (fun query -> check_equal "blank query rejected" true
    (invalid (Search_query.of_strings [query]))) [""; " \n"];
  List.iter (fun limit -> check_equal "limit rejected" true
    (invalid (Store_sqlite.search_products store ~user ~query:(parsed "whey") ~limit))) [0;501]

let test_isolation () =
  let store, alice, bob = Test_support.store_with_two_users () in
  let product = save store alice input in
  check_equal "foreign search empty" [] (search store bob "whey");
  List.iter (fun id -> check_equal "unknown or foreign update hidden" true
    (hidden (Store_sqlite.save_product store ~user:bob ~id:(Some id) input)))
    [product.id; Product_id.fresh ()];
  check_equal "no accidental insert" [] (search store bob "whey");
  check_equal "owner unchanged" [product] (search store alice "whey")

let test_literal_search_and_order () =
  let store, user = Test_support.store_with_user () in
  let a = save store user { input with name = "A whey" } in
  let b = save store user { input with name = "A whey" } in
  let z = save store user { input with name = "Z whey" } in
  let expected = List.sort (fun (a : Product.t) b -> String.compare (Product_id.to_string a.id) (Product_id.to_string b.id)) [a;b] in
  check_equal "same names distinct" false (a.id = b.id);
  check_equal "stable name/id order" (expected @ [z]) (search store user "WHEY");
  check_equal "limit after matching and ordering" [List.hd expected] (search store user ~limit:1 "whey");
  List.iter (fun (name, basis, source, query) ->
    let p = save store user { input with name; basis; source } in
    check_equal ("literal " ^ query) [p] (search store user query))
    ["100% food", "40 g", "label", "100";
     "under_score", "40 g", "label", "score";
     "Baker's food", "40 g", "label", "Baker's";
     "Polarbröd", "40 g", "label", "POLARBRÖD";
     "Basis match", "one portion", "label", "PORTION";
     "Source match", "40 g", "estimated strawberry", "STRAWBERRY"];
  check_equal "no matches" [] (search store user "nonexistent")

let test_storage_errors () =
  Test_support.with_exclusive_lock (fun store user -> save store user input)
    (fun store user product ->
      let sanitized = function Error (Error.Storage_error "SQLite operation failed") -> true | _ -> false in
      check_equal "locked insert" true (sanitized (Store_sqlite.save_product store ~user ~id:None input));
      check_equal "locked update" true (sanitized (Store_sqlite.save_product store ~user ~id:(Some product.id) input));
      check_equal "locked search" true (sanitized (Store_sqlite.search_products store ~user ~query:(parsed "whey") ~limit:100)))

let () = Alcotest.run "product store" ["references", List.map (fun (name, test) -> Alcotest.test_case name `Quick test)
  ["save and replace", test_save_and_replace; "validation", test_validation;
   "isolation", test_isolation; "literal search and order", test_literal_search_and_order;
   "sanitized storage errors", test_storage_errors]]
