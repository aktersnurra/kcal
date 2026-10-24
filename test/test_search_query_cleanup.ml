module Compiler = Search_query_fault_injected
module Fault = Compiler.Sqlite3

let check_failure finalize_fault close_fault fail_step () =
  Fault.finalize_fault := finalize_fault;
  Fault.close_fault := close_fault;
  Fault.fail_step := fail_step;
  Fault.finalized := 0;
  Fault.closed := 0;
  let result = Compiler.of_strings ["private query text"] in
  Alcotest.(check bool) "sanitized storage failure, no exception" true
    (match result with
     | Error (Error.Storage_error "search query tokenization failed") -> true
     | _ -> false);
  Alcotest.(check bool) "statement cleanup attempted" true (!Fault.finalized > 0);
  Alcotest.(check int) "database cleanup attempted once" 1 !Fault.closed

let () =
  let open Fault in
  Alcotest.run "search query cleanup"
    ["SQLite failures", List.map (fun (name,finalize,close,step) ->
       Alcotest.test_case name `Quick (check_failure finalize close step))
       ["finalize return code", Return_failure, No_fault, false;
        "finalize exception", Raise_failure, No_fault, false;
        "close false", No_fault, Return_failure, false;
        "close exception", No_fault, Raise_failure, false;
        "both cleanup exceptions", Raise_failure, Raise_failure, false;
        "failed step and finalize", Return_failure, No_fault, true]]
