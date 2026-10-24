module Compiler = Search_query_fault_injected
module Fault = Compiler.Sqlite3

let check_failure inject () =
  let db = Sqlite3.db_open ":memory:" in
  Fun.protect ~finally:(fun () -> assert (Sqlite3.db_close db)) (fun () ->
    let reset () =
      Fault.finalize_fault := Fault.No_fault;
      List.iter (fun flag -> flag := false)
        [Fault.fail_step; Fault.fail_prepare; Fault.fail_bind; Fault.fail_setup; Fault.fail_cleanup]
    in
    reset ();
    ignore (Result.get_ok (Compiler.of_strings db ["initial"]));
    Fault.deletes := 0;
    inject ();
    let result = Compiler.of_strings db ["private query text"] in
    reset ();
    Alcotest.(check bool) "sanitized storage failure, no exception" true
      (match result with Error (Error.Storage_error "search query tokenization failed") -> true | _ -> false);
    Alcotest.(check int) "compiler never opens" 0 !Fault.opened;
    Alcotest.(check int) "compiler never closes" 0 !Fault.closed;
    let stmt = Sqlite3.prepare db "SELECT count(*) FROM temp.kcal_search_query_input" in
    assert (Sqlite3.step stmt = Sqlite3.Rc.ROW);
    Alcotest.(check int) "no retained rows after handled failure" 0 (Sqlite3.column_int stmt 0);
    Sqlite3.Rc.check (Sqlite3.finalize stmt);
    Alcotest.(check string) "owner usable, no stale terms" "\"fresh\""
      (Compiler.fts5_match_expression (Result.get_ok (Compiler.of_strings db ["fresh"]))))

let () =
  let open Fault in
  Alcotest.run "search query cleanup"
    ["SQLite failures", List.map (fun (name,inject) ->
      Alcotest.test_case name `Quick (check_failure inject))
      ["finalize return code", (fun () -> finalize_fault := Return_failure);
       "finalize exception", (fun () -> finalize_fault := Raise_failure);
       "failed step and finalize", (fun () -> fail_step := true; finalize_fault := Return_failure);
       "prepare", (fun () -> fail_prepare := true);
       "bind", (fun () -> fail_bind := true);
       "setup", (fun () -> fail_setup := true);
       "cleanup", (fun () -> fail_cleanup := true)]]
