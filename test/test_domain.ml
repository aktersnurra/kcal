let valid_meal () =
  Meal.
    {
      description = "Soup";
      calories_kcal = 250;
      protein_g = 12.0;
      carbs_g = None;
      fat_g = None;
      confidence = None;
      estimate_source = None;
      notes = None;
      eaten_at = None;
    }

let test_rejects_negative_meal_calories () =
  let input = Meal.{ (valid_meal ()) with calories_kcal = -1 } in
  Alcotest.(check bool) "invalid" true
    (Result.is_error (Meal.validate_create input))

let test_rejects_blank_meal_description () =
  let input = Meal.{ (valid_meal ()) with description = "  \t" } in
  Alcotest.(check bool) "invalid" true
    (Result.is_error (Meal.validate_create input))

let test_rejects_negative_meal_macros () =
  let input = Meal.{ (valid_meal ()) with protein_g = -0.1 } in
  Alcotest.(check bool) "invalid protein" true
    (Result.is_error (Meal.validate_create input));
  let input = Meal.{ (valid_meal ()) with carbs_g = Some (-0.1) } in
  Alcotest.(check bool) "invalid carbs" true
    (Result.is_error (Meal.validate_create input));
  let input = Meal.{ (valid_meal ()) with fat_g = Some (-0.1) } in
  Alcotest.(check bool) "invalid fat" true
    (Result.is_error (Meal.validate_create input))

let test_rejects_invalid_confidence () =
  List.iter
    (fun confidence ->
      let input = Meal.{ (valid_meal ()) with confidence = Some confidence } in
      Alcotest.(check bool) "invalid" true
        (Result.is_error (Meal.validate_create input)))
    [ -0.1; 1.1 ]

let test_accepts_valid_meal () =
  Alcotest.(check bool) "valid" true
    (Result.is_ok (Meal.validate_create (valid_meal ())))

let test_rejects_nonpositive_manual_weight () =
  Alcotest.(check bool) "invalid" true
    (Result.is_error (Weigh_in.validate_manual_create None 0.0))

let test_accepts_positive_manual_weight () =
  Alcotest.(check bool) "valid" true
    (Result.is_ok (Weigh_in.validate_manual_create None 79.6))

let test_uuid_wrappers_accept_only_uuid_values () =
  let id = User_id.fresh () in
  Alcotest.(check bool) "generated UUID is accepted" true
    (Option.is_some (User_id.of_string (User_id.to_string id)));
  Alcotest.(check bool) "malformed UUID is rejected" true
    (Option.is_none (Meal_id.of_string "not-a-uuid"))

let test_patch_defaults_leave_values_unchanged () =
  let meal = Meal.empty_patch in
  Alcotest.(check bool) "meal description unchanged" true
    (Option.is_none meal.description);
  let weight = Weigh_in.empty_patch in
  Alcotest.(check bool) "weight unchanged" true
    (Option.is_none weight.weight_kg)

let test_source_strings () =
  Alcotest.(check string) "manual" "manual"
    (Weigh_in.source_to_string Weigh_in.Manual);
  Alcotest.(check string) "withings model" "withings"
    (Weigh_in.source_to_string Weigh_in.Withings)

let test_parses_offset_datetime_as_utc () =
  match Time.parse_offset_datetime "2026-01-02T03:04:05+02:00" with
  | Error _ -> Alcotest.fail "valid offset datetime rejected"
  | Ok time ->
      Alcotest.(check string) "UTC" "2026-01-02T01:04:05Z"
        (Time.to_utc_string time)

let test_rejects_malformed_or_no_offset_datetime () =
  List.iter
    (fun input ->
      Alcotest.(check bool) "invalid" true
        (Result.is_error (Time.parse_offset_datetime input)))
    [ "not-a-timestamp"; "2026-01-02T03:04:05"; "2026-01-02T03:04:05-00:00" ]

let () =
  Alcotest.run "domain"
    [
      ( "validation",
        [
          Alcotest.test_case "rejects negative meal calories" `Quick
            test_rejects_negative_meal_calories;
          Alcotest.test_case "rejects blank meal descriptions" `Quick
            test_rejects_blank_meal_description;
          Alcotest.test_case "rejects negative meal macros" `Quick
            test_rejects_negative_meal_macros;
          Alcotest.test_case "rejects invalid confidence" `Quick
            test_rejects_invalid_confidence;
          Alcotest.test_case "accepts valid meal" `Quick test_accepts_valid_meal;
          Alcotest.test_case "rejects nonpositive manual weight" `Quick
            test_rejects_nonpositive_manual_weight;
          Alcotest.test_case "accepts positive manual weight" `Quick
            test_accepts_positive_manual_weight;
          Alcotest.test_case "UUID wrappers validate UUID values" `Quick
            test_uuid_wrappers_accept_only_uuid_values;
          Alcotest.test_case "patch defaults leave values unchanged" `Quick
            test_patch_defaults_leave_values_unchanged;
          Alcotest.test_case "source strings" `Quick test_source_strings;
          Alcotest.test_case "parses offset datetimes as UTC" `Quick
            test_parses_offset_datetime_as_utc;
          Alcotest.test_case "rejects malformed or no-offset datetimes" `Quick
            test_rejects_malformed_or_no_offset_datetime;
        ] );
    ]
