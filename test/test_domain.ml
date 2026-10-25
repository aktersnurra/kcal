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
  Alcotest.(check bool)
    "invalid" true
    (Result.is_error (Meal.Create.of_raw input))

let test_rejects_blank_meal_description () =
  let input = Meal.{ (valid_meal ()) with description = "  \t" } in
  Alcotest.(check bool)
    "invalid" true
    (Result.is_error (Meal.Create.of_raw input))

let test_rejects_negative_meal_macros () =
  let input = Meal.{ (valid_meal ()) with protein_g = -0.1 } in
  Alcotest.(check bool)
    "invalid protein" true
    (Result.is_error (Meal.Create.of_raw input));
  let input = Meal.{ (valid_meal ()) with carbs_g = Some (-0.1) } in
  Alcotest.(check bool)
    "invalid carbs" true
    (Result.is_error (Meal.Create.of_raw input));
  let input = Meal.{ (valid_meal ()) with fat_g = Some (-0.1) } in
  Alcotest.(check bool)
    "invalid fat" true
    (Result.is_error (Meal.Create.of_raw input))

let test_rejects_invalid_confidence () =
  List.iter
    (fun confidence ->
      let input = Meal.{ (valid_meal ()) with confidence = Some confidence } in
      Alcotest.(check bool)
        "invalid" true
        (Result.is_error (Meal.Create.of_raw input)))
    [ -0.1; 1.1 ]

let test_accepts_valid_meal () =
  Alcotest.(check bool)
    "valid" true
    (Result.is_ok (Meal.Create.of_raw (valid_meal ())))

let test_rejects_nonpositive_manual_weight () =
  Alcotest.(check bool)
    "invalid" true
    (Result.is_error
       (Weigh_in.Create.of_raw { measured_at = None; weight_kg = 0.0 }))

let test_accepts_positive_manual_weight () =
  Alcotest.(check bool)
    "valid" true
    (Result.is_ok
       (Weigh_in.Create.of_raw { measured_at = None; weight_kg = 79.6 }))

let test_uuid_wrappers_accept_only_uuid_values () =
  let id = User_id.fresh () in
  Alcotest.(check bool)
    "generated UUID is accepted" true
    (Option.is_some (User_id.of_string (User_id.to_string id)));
  Alcotest.(check bool)
    "malformed UUID is rejected" true
    (Option.is_none (Meal_id.of_string "not-a-uuid"))

let test_patch_defaults_leave_values_unchanged () =
  let meal = Meal.empty_patch in
  Alcotest.(check bool)
    "meal description unchanged" true
    (Option.is_none meal.description);
  let weight = Weigh_in.empty_patch in
  Alcotest.(check bool)
    "weight unchanged" true
    (Option.is_none weight.weight_kg)

let test_source_strings () =
  Alcotest.(check string)
    "manual" "manual"
    (Weigh_in.source_to_string Weigh_in.Manual);
  Alcotest.(check string)
    "withings model" "withings"
    (Weigh_in.source_to_string Weigh_in.Withings)

let test_parses_offset_datetime_as_utc () =
  match Time.parse_offset_datetime "2026-01-02T03:04:05+02:00" with
  | Error _ -> Alcotest.fail "valid offset datetime rejected"
  | Ok time ->
      Alcotest.(check string)
        "UTC" "2026-01-02T01:04:05Z" (Time.to_utc_string time)

let test_rejects_malformed_or_no_offset_datetime () =
  List.iter
    (fun input ->
      Alcotest.(check bool)
        "invalid" true
        (Result.is_error (Time.parse_offset_datetime input)))
    [ "not-a-timestamp"; "2026-01-02T03:04:05"; "2026-01-02T03:04:05-00:00" ]

let test_rejects_nonfinite_inputs () =
  List.iter
    (fun value ->
      Alcotest.(check bool)
        "meal rejects nonfinite protein" true
        (Result.is_error
           (Meal.Create.of_raw { (valid_meal ()) with protein_g = value }));
      Alcotest.(check bool)
        "weight rejects nonfinite" true
        (Result.is_error
           (Weigh_in.Create.of_raw { measured_at = None; weight_kg = value })))
    [ infinity; neg_infinity; nan ]

let%hegel_test meal_optional_macros_obey_finite_nonnegative_contract tc =
  let protein_g = Hegel.draw tc (Hegel.floats ()) in
  let carbs_g = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let fat_g = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let valid x = Float.is_finite x && x >= 0.0 in
  let optional = function None -> true | Some x -> valid x in
  assert (
    Bool.equal
      (Result.is_ok
         (Meal.Create.of_raw { (valid_meal ()) with protein_g; carbs_g; fat_g }))
      (valid protein_g && optional carbs_g && optional fat_g))

let%hegel_test scalar_constructors_accept_exactly_their_domains tc =
  let calories = Hegel.draw tc (Hegel.integers ()) in
  let value = Hegel.draw tc (Hegel.floats ()) in
  let finite = Float.is_finite value in
  assert (
    Bool.equal
      (Result.is_ok (Nutrition.Calories.of_int calories))
      (calories >= 0));
  assert (
    Bool.equal
      (Result.is_ok (Nutrition.Grams.of_float value))
      (finite && value >= 0.0));
  assert (
    Bool.equal
      (Result.is_ok (Nutrition.Kilograms.of_float value))
      (finite && value > 0.0));
  assert (
    Bool.equal
      (Result.is_ok (Nutrition.Confidence.of_float value))
      (finite && value >= 0.0 && value <= 1.0));
  assert (
    Bool.equal
      (Result.is_ok (Summary.Protein_target.of_g value))
      (finite && value > 0.0))

let%hegel_test meal_create_and_patch_share_numeric_rules tc =
  let calories_kcal = Hegel.draw tc (Hegel.integers ()) in
  let protein_g = Hegel.draw tc (Hegel.floats ()) in
  let carbs_g = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let fat_g = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let confidence = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let create =
    Meal.Create.of_raw
      {
        (valid_meal ()) with
        calories_kcal;
        protein_g;
        carbs_g;
        fat_g;
        confidence;
      }
  in
  let patch =
    Meal.Patch.of_raw
      {
        Meal.empty_patch with
        calories_kcal = Some calories_kcal;
        protein_g = Some protein_g;
        carbs_g;
        fat_g;
        confidence;
      }
  in
  assert (Bool.equal (Result.is_ok create) (Result.is_ok patch))

let test_all_numeric_inputs_reject_nonfinite () =
  List.iter
    (fun value ->
      let rejected name result =
        Alcotest.(check bool)
          name true
          (match result with
          | Error (Error.Invalid_field { reason = Nonfinite; _ }) -> true
          | _ -> false)
      in
      rejected "meal protein patch"
        (Meal.Patch.of_raw { Meal.empty_patch with protein_g = Some value });
      rejected "meal carbs create"
        (Meal.Create.of_raw { (valid_meal ()) with carbs_g = Some value });
      rejected "meal fat patch"
        (Meal.Patch.of_raw { Meal.empty_patch with fat_g = Some value });
      rejected "meal confidence create"
        (Meal.Create.of_raw { (valid_meal ()) with confidence = Some value });
      rejected "meal confidence patch"
        (Meal.Patch.of_raw { Meal.empty_patch with confidence = Some value });
      rejected "manual weight patch"
        (Weigh_in.Patch.of_raw
           { Weigh_in.empty_patch with weight_kg = Some value });
      rejected "import weight"
        (Weigh_in.Import.of_raw
           {
             external_id = "account:group";
             measured_at = Ptime.epoch;
             weight_kg = value;
           });
      rejected "product"
        (Product.Create.of_raw
           {
             name = "p";
             basis = "b";
             source = "s";
             calories_kcal = 0;
             protein_g = value;
             carbs_g = None;
             fat_g = None;
           }))
    [ infinity; neg_infinity; nan ]

let () =
  Alcotest.run "domain"
    [
      ( "validation",
        [
          Alcotest.test_case "rejects negative meal calories" `Quick
            test_rejects_negative_meal_calories;
          Alcotest.test_case "scalar domains property" `Quick
            scalar_constructors_accept_exactly_their_domains;
          Alcotest.test_case "create patch numeric consistency property" `Quick
            meal_create_and_patch_share_numeric_rules;
          Alcotest.test_case "nonfinite create patch import" `Quick
            test_all_numeric_inputs_reject_nonfinite;
          Alcotest.test_case "optional macro property" `Quick
            meal_optional_macros_obey_finite_nonnegative_contract;
          Alcotest.test_case "rejects nonfinite inputs" `Quick
            test_rejects_nonfinite_inputs;
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
