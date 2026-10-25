module Domain_error = Error
open Base
module Error = Domain_error

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
    ~f:(fun confidence ->
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
    (Weigh_in.source_to_string
       (Weigh_in.Withings
          (Option.value_exn
             (Result.ok
                (Weigh_in.Imported_identity.of_external_id "account:group")))))

let test_parses_offset_datetime_as_utc () =
  match Time.parse_offset_datetime "2026-01-02T03:04:05+02:00" with
  | Error _ -> Alcotest.fail "valid offset datetime rejected"
  | Ok time ->
      Alcotest.(check string)
        "UTC" "2026-01-02T01:04:05Z" (Time.to_utc_string time)

let test_rejects_malformed_or_no_offset_datetime () =
  List.iter
    ~f:(fun input ->
      Alcotest.(check bool)
        "invalid" true
        (Result.is_error (Time.parse_offset_datetime input)))
    [ "not-a-timestamp"; "2026-01-02T03:04:05"; "2026-01-02T03:04:05-00:00" ]

let test_rejects_nonfinite_inputs () =
  List.iter
    ~f:(fun value ->
      Alcotest.(check bool)
        "meal rejects nonfinite protein" true
        (Result.is_error
           (Meal.Create.of_raw { (valid_meal ()) with protein_g = value }));
      Alcotest.(check bool)
        "weight rejects nonfinite" true
        (Result.is_error
           (Weigh_in.Create.of_raw { measured_at = None; weight_kg = value })))
    [ Float.infinity; Float.neg_infinity; Float.nan ]

let%hegel_test meal_optional_macros_obey_finite_nonnegative_contract tc =
  let protein_g = Hegel.draw tc (Hegel.floats ()) in
  let carbs_g = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let fat_g = Hegel.draw tc (Hegel.optional (Hegel.floats ())) in
  let valid x = Float.is_finite x && Float.(x >= 0.0) in
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
      (finite && Float.(value >= 0.0)));
  assert (
    Bool.equal
      (Result.is_ok (Nutrition.Kilograms.of_float value))
      (finite && Float.(value > 0.0)));
  assert (
    Bool.equal
      (Result.is_ok (Nutrition.Confidence.of_float value))
      (finite && Float.(value >= 0.0) && Float.(value <= 1.0)));
  assert (
    Bool.equal
      (Result.is_ok (Summary.Protein_target.of_g value))
      (finite && Float.(value > 0.0)))

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
    ~f:(fun value ->
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
    [ Float.infinity; Float.neg_infinity; Float.nan ]

module Calendar_properties = struct
  module Domain_error = Error
  open Base
  module Error = Domain_error
  module Calendar = Calendar_test_support

  let ok = function
    | Ok value -> value
    | Error error -> failwith (Error.to_string error)

  let%hegel_test date_round_trips_through_its_string tc =
    let date = Calendar.draw_date tc in
    assert (Date.equal (ok (Date.of_string (Date.to_string date))) date)

  let%hegel_test date_addition_matches_gregorian_ordinals tc =
    let date = Calendar.draw_date tc in
    let days = Hegel.draw tc (Hegel.integers ()) in
    let expected = Int64.(of_int (Calendar.ordinal date) + of_int days) in
    let representable =
      Int64.(expected >= 0L && expected <= of_int Calendar.last_ordinal)
    in
    match Date.add_days date days with
    | Ok added ->
        assert representable;
        assert (Int64.equal (Int64.of_int (Calendar.ordinal added)) expected)
    | Error (Error.Calendar_date_addition _) -> assert (not representable)
    | Error _ -> assert false

  let%hegel_test date_range_lists_consecutive_dates_from_first_to_last tc =
    let first = Calendar.draw_date tc in
    let available = Calendar.last_ordinal - Calendar.ordinal first + 1 in
    let length =
      Hegel.draw tc
        (Hegel.integers ~min_value:1
           ~max_value:(Int.min Date_range.max_day_count available)
           ())
    in
    let last = ok (Date.add_days first (length - 1)) in
    let range = ok (Date_range.make ~first ~last) in
    let dates = Date_range.dates range in
    assert (List.length dates = length && Date_range.day_count range = length);
    List.iteri dates ~f:(fun index date ->
        assert (Calendar.ordinal date = Calendar.ordinal first + index));
    assert (Date.equal (List.last_exn dates) last);
    if available > Date_range.max_day_count then
      assert (
        Result.is_error
          (Date_range.make ~first
             ~last:(ok (Date.add_days first Date_range.max_day_count))));
    assert (
      length = 1 || Result.is_error (Date_range.make ~first:last ~last:first))

  let test_calendar_endpoints () =
    List.iter
      [
        "0000-01-01";
        "0000-02-29";
        "0001-01-01";
        "1900-02-28";
        "2000-02-29";
        "9999-12-31";
      ] ~f:(fun text ->
        assert (String.equal text (Date.to_string (ok (Date.of_string text)))));
    let first = ok (Date.of_string "0000-01-01") in
    assert (Result.is_error (Date.add_days first (-1)));
    assert (Result.is_error (Date.add_days Calendar.last_date 1));
    List.iter [ first; Calendar.last_date ] ~f:(fun date ->
        let range = ok (Date_range.make ~first:date ~last:date) in
        assert (List.equal Date.equal (Date_range.dates range) [ date ]));
    assert (
      Date.equal
        (ok (Date.add_days first Calendar.last_ordinal))
        Calendar.last_date)
end

let () =
  Alcotest.run "domain"
    [
      ( "calendar",
        [
          Alcotest.test_case "full-calendar roundtrip property" `Quick
            Calendar_properties.date_round_trips_through_its_string;
          Alcotest.test_case "date addition ordinal property" `Quick
            Calendar_properties.date_addition_matches_gregorian_ordinals;
          Alcotest.test_case "inclusive date range property" `Quick
            Calendar_properties
            .date_range_lists_consecutive_dates_from_first_to_last;
          Alcotest.test_case "calendar endpoints and leap days" `Quick
            Calendar_properties.test_calendar_endpoints;
        ] );
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
