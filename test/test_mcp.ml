let response_has_error_code code = function
  | `Assoc fields ->
      (match List.assoc_opt "error" fields with
      | Some (`Assoc error) -> List.assoc_opt "code" error = Some (`Int code)
      | _ -> false)
  | _ -> false

let test_record_meal_rejects_user_id () =
  let store, user = Test_support.store_with_user () in
  let request = `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String "tools/call");
    ("params", `Assoc [ ("name", `String "record_meal");
      ("arguments", `Assoc [ ("user_id", `String "other-user");
        ("description", `String "Soup"); ("calories_kcal", `Int 250);
        ("protein_g", `Float 12.) ]) ]) ] in
  Alcotest.(check bool) "invalid parameters" true
    (response_has_error_code (-32602) (Mcp.handle ~service:(Service.make ~store) ~user request))

let test_mcp_rejects_bad_jsonrpc_and_provenance_fields () =
  let store, user = Test_support.store_with_user () in
  let service = Service.make ~store in
  let bad_version = `Assoc [ ("jsonrpc", `String "1.0"); ("method", `String "tools/list") ] in
  let source_argument = `Assoc [ ("jsonrpc", `String "2.0"); ("method", `String "tools/call"); ("params", `Assoc [ ("name", `String "record_weight"); ("arguments", `Assoc [ ("weight_kg", `Float 70.); ("source", `String "withings") ]) ]) ] in
  Alcotest.(check bool) "version" true (response_has_error_code (-32600) (Mcp.handle ~service ~user bad_version));
  Alcotest.(check bool) "provenance" true (response_has_error_code (-32602) (Mcp.handle ~service ~user source_argument))

let test_tool_list_is_exact () =
  let store, user = Test_support.store_with_user () in
  let response = Mcp.handle ~service:(Service.make ~store) ~user (`Assoc [ ("jsonrpc", `String "2.0"); ("method", `String "tools/list") ]) in
  let names = match response with
    | `Assoc fields -> (match List.assoc_opt "result" fields with Some (`Assoc result) -> (match List.assoc_opt "tools" result with Some (`List tools) -> List.filter_map (function `Assoc tool -> (match List.assoc_opt "name" tool with Some (`String name) -> Some name | _ -> None) | _ -> None) tools | _ -> []) | _ -> [])
    | _ -> [] in
  Alcotest.(check (list string)) "approved tools"
    [ "record_meal"; "get_meal"; "query_meals"; "update_meal"; "delete_meal"; "record_weight"; "get_weight"; "query_weights"; "update_weight"; "delete_weight"; "begin_withings_connection"; "get_withings_status"; "disconnect_withings" ] names

let () = Alcotest.run "mcp" [ ("boundary", [ Alcotest.test_case "rejects user id" `Quick test_record_meal_rejects_user_id; Alcotest.test_case "rejects bad JSON-RPC and provenance" `Quick test_mcp_rejects_bad_jsonrpc_and_provenance_fields; Alcotest.test_case "lists approved tools" `Quick test_tool_list_is_exact ]) ]
