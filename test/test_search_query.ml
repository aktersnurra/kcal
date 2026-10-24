let check label expected actual = Alcotest.(check string) label expected actual
let with_db f =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect ~finally:(fun () -> assert (Sqlite3.db_close db)) (fun () -> f db)
let compile_on db texts = Result.get_ok (Search_query.of_strings db texts) |> Search_query.fts5_match_expression
let compile texts = with_db (fun db -> compile_on db texts)
let test_compile () =
  check "plain text operators and punctuation" "\"baker\" OR \"drop\" OR \"not\" OR \"or\" OR \"s\" OR \"table\" OR \"whey\""
    (compile ["Baker's \"whey\"* OR NOT; DROP-table"]);
  check "duplicates do not weight words" "\"protein\" OR \"whey\"" (compile ["Whey protein"; "whey"; "protein"]);
  check "Swedish case duplicates" "\"bröd\" OR \"räka\"" (compile ["BRÖD RÄKA"; "bröd räka"]);
  check "Unicode separators are independent terms" "\"chicken\" OR \"rice\""
    (compile ["chicken—rice"; "CHICKEN RICE"; "chicken rice"; "chicken–rice"]);
  check "Unicode case normalization before deduplication" "\"bröd\" OR \"whey\" OR \"éclair\""
    (compile ["BRÖD WHEY ÉCLAIR"; "bröd whey éclair"]);
  check "diacritics remain distinct" "\"brod\" OR \"bröd\"" (compile ["brod BRÖD"])
let test_invalid () =
  List.iter (fun alternatives -> Alcotest.(check bool) "invalid" true
    (match with_db (fun db -> Search_query.of_strings db alternatives) with Error (Error.Invalid_input _) -> true | _ -> false))
    [[]; [""]; ["  \n"]; ["whey"; ""]; ["***'_%\" -;()"]];
  ignore (compile ["🥣"])
let count db sql =
  let stmt = Sqlite3.prepare db sql in
  Fun.protect ~finally:(fun () -> ignore (Sqlite3.finalize stmt)) (fun () ->
    assert (Sqlite3.step stmt = Sqlite3.Rc.ROW);
    Sqlite3.column_int stmt 0)

let test_owner_reuse () = with_db (fun db ->
  Sqlite3.Rc.check (Sqlite3.exec db
    "CREATE TABLE sentinel(value); INSERT INTO sentinel VALUES (42); CREATE TABLE kcal_search_query_input(text); INSERT INTO kcal_search_query_input VALUES ('main untouched'); CREATE TABLE kcal_search_query_terms(term); INSERT INTO kcal_search_query_terms VALUES ('wrong schema')");
  let schema = count db "PRAGMA main.schema_version" in
  check "first" "\"first\"" (compile_on db ["first"]);
  Alcotest.(check int) "owner has reusable tokenizer" 1
    (count db "SELECT count(*) FROM temp.sqlite_schema WHERE name='kcal_search_query_input'");
  let temp_schema = count db "PRAGMA temp.schema_version" in
  let empty () = Alcotest.(check int) "no retained query rows" 0
    (count db "SELECT count(*) FROM temp.kcal_search_query_input") in
  empty ();
  check "distinct call, no stale terms or main vocab" "\"second\"" (compile_on db ["SECOND"]);
  List.iter (fun alternatives ->
    Alcotest.(check bool) "invalid" true
      (match Search_query.of_strings db alternatives with Error (Error.Invalid_input _) -> true | _ -> false);
    empty ()) [["private"; ""]; []; ["***"]];
  check "after invalid" "\"third\"" (compile_on db ["third"]);
  empty ();
  Alcotest.(check int) "TEMP schema reused, not recreated" temp_schema (count db "PRAGMA temp.schema_version");
  Alcotest.(check int) "main schema unchanged" schema (count db "PRAGMA main.schema_version");
  Alcotest.(check int) "app data unchanged" 42 (count db "SELECT value FROM sentinel");
  Alcotest.(check int) "main namesake untouched" 1 (count db "SELECT count(*) FROM main.kcal_search_query_input WHERE text='main untouched'");
  Alcotest.(check int) "main vocab namesake untouched" 1
    (count db "SELECT count(*) FROM main.kcal_search_query_terms WHERE term='wrong schema'"))

let test_real_sqlite_failure () = with_db (fun db ->
  ignore (compile_on db ["initial"]);
  Sqlite3.Rc.check (Sqlite3.exec db
    "DROP TABLE temp.kcal_search_query_terms; CREATE TEMP TABLE kcal_search_query_terms(wrong_column)");
  Alcotest.(check bool) "real failed SELECT is sanitized" true
    (match Search_query.of_strings db ["private"] with
     | Error (Error.Storage_error "search query tokenization failed") -> true | _ -> false);
  Alcotest.(check int) "failed SELECT input cleared" 0
    (count db "SELECT count(*) FROM temp.kcal_search_query_input");
  Sqlite3.Rc.check (Sqlite3.exec db "DROP TABLE temp.kcal_search_query_terms");
  check "owner recovers" "\"fresh\"" (compile_on db ["fresh"]))

let test_independent_owners () = with_db (fun second ->
  with_db (fun first ->
    check "first owner" "\"alpha\"" (compile_on first ["alpha"]);
    Alcotest.(check int) "second not initialized" 0
      (count second "SELECT count(*) FROM temp.sqlite_schema");
    check "second owner" "\"beta\"" (compile_on second ["beta"]));
  check "second survives first close" "\"gamma\"" (compile_on second ["gamma"]);
  with_db (fun fresh -> Alcotest.(check int) "fresh owner has no resources" 0
    (count fresh "SELECT count(*) FROM temp.sqlite_schema")))

let () = Alcotest.run "search query" ["plain text", [Alcotest.test_case "real SQLite failure" `Quick test_real_sqlite_failure; Alcotest.test_case "independent owners" `Quick test_independent_owners; Alcotest.test_case "owner reuse" `Quick test_owner_reuse; Alcotest.test_case "safe distinct words" `Quick test_compile; Alcotest.test_case "invalid alternatives" `Quick test_invalid]]
