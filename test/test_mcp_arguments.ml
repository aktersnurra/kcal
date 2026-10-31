open Base

let parse name fields =
  Mcp_arguments.parse ~now:Ptime.epoch
    ~default_zone:(fun () -> Ok Time.utc)
    ~name
    ~compile_search:(fun _ -> Alcotest.fail "unexpected search compilation")
    (`Assoc fields)

let test_query_bounds_are_validated () =
  List.iter [ 0; -1; 501; Int.max_value ] ~f:(fun limit ->
      Alcotest.(check bool)
        "query limit rejected" true
        (Result.is_error (parse "query_weights" [ ("limit", `Int limit) ])));
  Alcotest.(check bool)
    "reversed times rejected" true
    (Result.is_error
       (parse "query_weights"
          [
            ("from", `String "2026-10-25T12:00:00Z");
            ("to", `String "2026-10-25T11:00:00Z");
          ]))

let test_query_defaults_and_endpoints () =
  List.iter
    [ []; [ ("limit", `Int 1) ]; [ ("limit", `Int 500) ] ]
    ~f:(fun fields ->
      match parse "query_weights" fields with
      | Ok (Mcp_arguments.Query_weights request) ->
          Alcotest.(check int)
            "validated limit"
            (match fields with [ (_, `Int limit) ] -> limit | _ -> 100)
            request.limit
      | _ -> Alcotest.fail "expected a validated weight query")

let () =
  Alcotest.run "Mcp_arguments"
    [
      ( "query",
        [
          Alcotest.test_case "validates bounds" `Quick
            test_query_bounds_are_validated;
          Alcotest.test_case "defaults and endpoints" `Quick
            test_query_defaults_and_endpoints;
        ] );
    ]
