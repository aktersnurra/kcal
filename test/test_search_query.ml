let check label expected actual = Alcotest.(check string) label expected actual
let compile texts = Result.get_ok (Search_query.of_strings texts) |> Search_query.fts5_match_expression
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
    (match Search_query.of_strings alternatives with Error (Error.Invalid_input _) -> true | _ -> false))
    [[]; [""]; ["  \n"]; ["whey"; ""]; ["***'_%\" -;()"]];
  ignore (compile ["🥣"])
let () = Alcotest.run "search query" ["plain text", [Alcotest.test_case "safe distinct words" `Quick test_compile; Alcotest.test_case "invalid alternatives" `Quick test_invalid]]
