open Base
open Hegel

let check = Alcotest.check

let json_rows =
  Alcotest.list
    (Alcotest.testable (Yojson.Safe.pretty_print ?std:None) Yojson.Safe.equal)

let product name =
  Product.
    {
      name;
      basis = "portion";
      serving = None;
      calories_kcal = 100;
      protein_g = 10.;
      carbs_g = None;
      fat_g = None;
      source = "label";
    }

let save db user name =
  Option.value_exn
    (Result.ok (Test_support.save_product_raw db ~user ~id:None (product name)))

let meal db user description =
  Option.value_exn
    (Result.ok
       (Test_support.create_meal_raw db ~user
          {
            (Test_support.meal_input ()) with
            description;
            estimate_source = None;
            notes = None;
          }))

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
      match List.Assoc.find response ~equal:String.equal "result" with
      | Some (`Assoc result) -> (
          match
            List.Assoc.find result ~equal:String.equal "structuredContent"
          with
          | Some (`Assoc fields) -> (
              match List.Assoc.find fields ~equal:String.equal key with
              | Some (`List rows) -> rows
              | _ -> Alcotest.fail "missing result rows")
          | _ -> Alcotest.fail "missing structured result")
      | _ -> Alcotest.fail "missing structured result")
  | _ -> Alcotest.fail "MCP request failed"

let alternatives xs = `List (List.map ~f:(fun s -> `String s) xs)

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
        ~f:(fun text ->
          ignore (save db user text);
          ignore (meal db user text))
        [ "rice chicken"; "chicken"; "rice"; "BRÖD WHEY"; "brod whey" ];
      List.iter
        ~f:(fun (tool, key, result_key) ->
          let search value =
            rows result_key (request db user tool key value [])
          in
          let expected = search (`String "chicken rice") in
          check Alcotest.int
            (tool ^ " reversed and partial overlap fixtures")
            3 (List.length expected);
          List.iter
            ~f:(fun text ->
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
        [ Mcp_json.product_json strong ]
        result)

let test_v3_backfill () =
  let path = Stdlib.Filename.temp_file "kcal-v3" ".sqlite" in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.Sys.remove path)
    (fun () ->
      let raw = Sqlite3.db_open path in
      Stdlib.Fun.protect
        ~finally:(fun () -> Test_support.close_database raw)
        (fun () ->
          List.iter
            ~f:(fun (v, sql) ->
              if v <= 3 then (
                Option.value_exn
                  (Result.ok (Migration.For_testing.execute raw sql));
                Option.value_exn
                  (Result.ok (Migration.For_testing.record_version raw v))))
            Migration.migrations;
          Option.value_exn
            (Result.ok
               (Migration.For_testing.execute raw
                  "INSERT INTO users VALUES \
                   ('12345678-1234-4234-8234-123456789abc','issuer','alice','2026-01-01T00:00:00Z'); \
                   INSERT INTO products VALUES \
                   ('22345678-1234-4234-8234-123456789abc','12345678-1234-4234-8234-123456789abc','rice \
                   chicken','100g',100,10,NULL,NULL,'label','2026-01-01T00:00:00Z','2026-01-01T00:00:00Z'); \
                   INSERT INTO meals VALUES \
                   ('32345678-1234-4234-8234-123456789abc','12345678-1234-4234-8234-123456789abc','2026-01-01T00:00:00Z','ris \
                   kyckling',100,10,NULL,NULL,NULL,NULL,NULL,'2026-01-01T00:00:00Z','2026-01-01T00:00:00Z',NULL); \
                   INSERT INTO meals VALUES \
                   ('42345678-1234-4234-8234-123456789abc','12345678-1234-4234-8234-123456789abc','2026-01-01T00:00:00Z','ris \
                   kyckling',100,10,NULL,NULL,NULL,NULL,NULL,'2026-01-01T00:00:00Z','2026-01-01T00:00:00Z','2026-01-01T00:00:00Z')")));
      let db =
        Option.value_exn (Result.ok (Store_sqlite.open_database ~path))
      in
      Stdlib.Fun.protect
        ~finally:(fun () ->
          Option.value_exn (Result.ok (Store_sqlite.close db)))
        (fun () ->
          let user =
            Option.value_exn
              (Result.ok
                 (Store_sqlite.resolve_user db ~issuer:"issuer" ~subject:"alice"))
          in
          check Alcotest.int "product backfilled" 1
            (List.length
               (rows "products"
                  (request db user "search_products" "query"
                     (`String "chicken rice") [])));
          check Alcotest.int "meal backfilled" 1
            (List.length
               (rows "meals"
                  (request db user "query_meals" "search"
                     (`String "kyckling ris") [])))))

let test_invalid_wire_forms () =
  Test_support.with_store (fun db user ->
      (* An explained tool error the model can act on, not a protocol error. *)
      let invalid = function
        | Ok (`Assoc fields) -> (
            match List.Assoc.find fields ~equal:String.equal "result" with
            | Some (`Assoc result) ->
                Option.equal Yojson.Safe.equal
                  (List.Assoc.find result ~equal:String.equal "isError")
                  (Some (`Bool true))
            | _ -> false)
        | _ -> false
      in
      List.iter
        ~f:(fun (tool, key) ->
          List.iter
            ~f:(fun value ->
              check Alcotest.bool "invalid search is an explained tool error"
                true
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
        ~f:(fun text ->
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
        Option.value_exn
          (Result.ok
             (Test_support.save_product_raw db ~user ~id:(Some p.id)
                (product "newword")))
      in
      check json_rows "replacement removes old terms" []
        (find_products "oldword");
      check json_rows "replacement indexes new terms"
        [ Mcp_json.product_json changed ]
        (find_products "newword");
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute
              (Store_sqlite.For_testing.raw_database db)
              "DELETE FROM products WHERE name = 'newword'"));
      check json_rows "physical product delete" [] (find_products "newword");
      let m = meal db user "oldmeal" in
      let find_meals text =
        rows "meals" (request db user "query_meals" "search" (`String text) [])
      in
      ignore
        (Option.value_exn
           (Result.ok
              (Test_support.update_only_item_raw db ~user m
                 Meal_item.{ empty_patch with description = Some "newmeal" })));
      check json_rows "meal update removes old terms" [] (find_meals "oldmeal");
      check Alcotest.int "meal update adds new terms" 1
        (List.length (find_meals "newmeal"));
      ignore (meal db other "newmeal");
      Option.value_exn (Result.ok (Store_sqlite.delete_meal db ~user m.id));
      check json_rows "soft delete and foreign hidden" [] (find_meals "newmeal");
      let n = meal db user "physicalmeal" in
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute
              (Store_sqlite.For_testing.raw_database db)
              ("DELETE FROM meal_items WHERE meal_id = '"
             ^ Meal_id.to_string n.id ^ "'; DELETE FROM meals WHERE id = '"
             ^ Meal_id.to_string n.id ^ "'")));
      check json_rows "physical meal delete" [] (find_meals "physicalmeal");
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute
              (Store_sqlite.For_testing.raw_database db)
              "INSERT INTO products_fts(products_fts) VALUES \
               ('integrity-check')"));
      Option.value_exn
        (Result.ok
           (Migration.For_testing.execute
              (Store_sqlite.For_testing.raw_database db)
              "INSERT INTO meal_items_fts(meal_items_fts) VALUES \
               ('integrity-check')")))

let test_meal_ranked_filters () =
  Test_support.with_two_users (fun db user other ->
      let create owner time description =
        let eaten_at =
          Option.value_exn (Result.ok (Time.parse_offset_datetime time))
        in
        Option.value_exn
          (Result.ok
             (Test_support.create_meal_raw db ~user:owner
                {
                  (Test_support.meal_input ~eaten_at ()) with
                  description;
                  estimate_source = None;
                  notes = None;
                }))
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
      Option.value_exn
        (Result.ok (Store_sqlite.delete_meal db ~user deleted.id));
      let value = alternatives [ "chicken rice"; "chicken"; "chicken rice" ] in
      let result =
        rows "meals"
          (request db user "query_meals" "search" value
             [ ("from", `String "2026-07-01T00:00:00Z"); ("limit", `Int 1) ])
      in
      check json_rows
        "ranking after owner date deleted filters and deduplication"
        [ Option.value_exn (Result.ok (Mcp_json.meal_json strong)) ]
        result)

let test_meal_stable_ties () =
  Test_support.with_store (fun db user ->
      let eaten_at =
        Option.value_exn
          (Result.ok (Time.parse_offset_datetime "2026-07-01T00:00:00Z"))
      in
      let create () =
        Option.value_exn
          (Result.ok
             (Test_support.create_meal_raw db ~user
                {
                  (Test_support.meal_input ~eaten_at ()) with
                  description = "chicken rice";
                }))
      in
      let a = create () and b = create () in
      let expected =
        List.sort [ a; b ] ~compare:(fun (a : Meal.t) b ->
            Meal_id.compare a.id b.id)
      in
      check json_rows "meal relevance ties use time then id"
        (List.map expected ~f:(fun meal ->
             Option.value_exn (Result.ok (Mcp_json.meal_json meal))))
        (rows "meals"
           (request db user "query_meals" "search" (`String "rice chicken") [])))

let vocabulary = [ "alpha"; "beta"; "gamma"; "delta" ]
let meal_id id = Option.value_exn (Meal_id.of_string id)
let product_id id = Option.value_exn (Product_id.of_string id)

let ids_of rows =
  List.sort ~compare:String.compare
    (List.map rows ~f:(function
      | `Assoc fields -> (
          match List.Assoc.find fields ~equal:String.equal "id" with
          | Some (`String id) -> id
          | _ -> Alcotest.fail "row without id")
      | _ -> Alcotest.fail "row is not an object"))

(* Independent model: an id -> words map per record kind. After every create,
   replace, update or delete, a one-word search must return exactly the live
   records whose words include it. *)
let%hegel_test search_index_follows_record_lifecycle tc =
  Test_support.with_store (fun db user ->
      let text () =
        String.concat ~sep:" "
          (List.map ~f:(List.nth_exn vocabulary)
             (draw tc
                (lists
                   (integers ~min_value:0
                      ~max_value:(List.length vocabulary - 1)
                      ())
                   ~min_size:1 ~max_size:3 ())))
      in
      let words text = String.split text ~on:' ' in
      let meals = Hashtbl.create (module String) in
      let products = Hashtbl.create (module String) in
      let expected table word =
        Hashtbl.fold table ~init:[] ~f:(fun ~key ~data acc ->
            if List.mem data word ~equal:String.equal then key :: acc else acc)
        |> List.sort ~compare:String.compare
      in
      let check_indexes () =
        List.iter vocabulary ~f:(fun word ->
            assert (
              List.equal String.equal (expected meals word)
                (ids_of
                   (rows "meals"
                      (request db user "query_meals" "search" (`String word) []))));
            assert (
              List.equal String.equal (expected products word)
                (ids_of
                   (rows "products"
                      (request db user "search_products" "query" (`String word)
                         [])))))
      in
      for _ = 1 to draw tc (integers ~min_value:1 ~max_value:12 ()) do
        let live table =
          Hashtbl.keys table |> List.sort ~compare:String.compare
        in
        let pick table = draw_silent tc (sampled_from (live table)) in
        let operation =
          draw_silent tc
            (sampled_from
               (List.concat
                  [
                    [ `Create_meal; `Save_product ];
                    (if Hashtbl.is_empty meals then []
                     else [ `Update_meal; `Delete_meal ]);
                    (if Hashtbl.is_empty products then []
                     else [ `Replace_product ]);
                  ]))
        in
        (match operation with
        | `Create_meal ->
            let description = text () in
            let created = meal db user description in
            Hashtbl.set meals
              ~key:(Meal_id.to_string created.id)
              ~data:(words description)
        | `Update_meal ->
            let id = pick meals in
            let description = text () in
            ignore
              (Option.value_exn
                 (Result.ok
                    (Result.bind
                       (Store_sqlite.get_meal db ~user (meal_id id))
                       ~f:(fun meal ->
                         Test_support.update_only_item_raw db ~user meal
                           Meal_item.
                             { empty_patch with description = Some description }))));
            Hashtbl.set meals ~key:id ~data:(words description)
        | `Delete_meal ->
            let id = pick meals in
            Option.value_exn
              (Result.ok (Store_sqlite.delete_meal db ~user (meal_id id)));
            Hashtbl.remove meals id
        | `Save_product ->
            let name = text () in
            let saved = save db user name in
            Hashtbl.set products
              ~key:(Product_id.to_string saved.id)
              ~data:(words name)
        | `Replace_product ->
            let id = pick products in
            let name = text () in
            ignore
              (Option.value_exn
                 (Result.ok
                    (Test_support.save_product_raw db ~user
                       ~id:(Some (product_id id))
                       (product name))));
            Hashtbl.set products ~key:id ~data:(words name));
        check_indexes ()
      done)

let () =
  Alcotest.run "ranked search"
    [
      ( "behavior",
        List.map
          ~f:(fun (n, t) -> Alcotest.test_case n `Quick t)
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
      ( "properties",
        [
          Alcotest.test_case "index follows record lifecycle" `Quick
            search_index_follows_record_lifecycle;
        ] );
    ]
