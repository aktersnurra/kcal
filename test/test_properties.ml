open Hegel

(* Any calendar date within [1000, 9999] fits Time.to_date_string's %04d-%02d-%02d
   shape and Time.parse_date's strict "YYYY-MM-DD" check exactly, so bounding the
   generators here avoids exercising unrelated formatting edge cases. *)
let year_gen = integers ~min_value:1000 ~max_value:9999 ()
let month_gen = integers ~min_value:1 ~max_value:12 ()
let day_gen = integers ~min_value:1 ~max_value:28 ()

let%hegel_test day_bounds_is_a_24_hour_window_containing_the_original_time tc =
  let year = draw tc year_gen in
  let month = draw tc month_gen in
  let day = draw tc day_gen in
  let hour = draw tc (integers ~min_value:0 ~max_value:23 ()) in
  let minute = draw tc (integers ~min_value:0 ~max_value:59 ()) in
  let second = draw tc (integers ~min_value:0 ~max_value:59 ()) in
  let time = Option.get (Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0))) in
  let start, stop = Time.day_bounds time in
  assert (Ptime.compare start time <= 0);
  assert (Ptime.compare time stop < 0);
  assert (Ptime.equal stop (Option.get (Ptime.add_span start (Ptime.Span.of_int_s 86400))));
  assert (Time.to_date_string start = Time.to_date_string time)

let%hegel_test parse_date_round_trips_through_to_date_string tc =
  let year = draw tc year_gen in
  let month = draw tc month_gen in
  let day = draw tc day_gen in
  let text = Printf.sprintf "%04d-%02d-%02d" year month day in
  match Time.parse_date text with
  | Ok time -> assert (Time.to_date_string time = text)
  | Error _ -> assert false

(* Generalizes the hand-picked "patch one field" examples in test_meal_store.ml:
   for any subset of description/calories_kcal/protein_g included in a patch,
   update_meal must change exactly those fields and leave the rest as they were. *)
let%hegel_test meal_patch_touches_only_the_included_fields tc =
  let store, user = Test_support.store_with_user () in
  let meal = Test_support.create_meal store user in
  let include_description = draw tc (booleans ()) in
  let include_calories = draw tc (booleans ()) in
  let include_protein = draw tc (booleans ()) in
  let new_description = "item " ^ draw tc (text ()) in
  let new_calories = draw tc (integers ~min_value:0 ~max_value:5000 ()) in
  let new_protein = float_of_int (draw tc (integers ~min_value:0 ~max_value:500 ())) in
  let patch =
    Meal.
      {
        empty_patch with
        description = (if include_description then Some new_description else None);
        calories_kcal = (if include_calories then Some new_calories else None);
        protein_g = (if include_protein then Some new_protein else None);
      }
  in
  let updated = Result.get_ok (Store_sqlite.update_meal store ~user meal.id patch) in
  assert (updated.description = if include_description then new_description else meal.description);
  assert (updated.calories_kcal = if include_calories then new_calories else meal.calories_kcal);
  assert (updated.protein_g = if include_protein then new_protein else meal.protein_g);
  (* Fields never included in this patch must never move, no matter what else did. *)
  assert (updated.carbs_g = meal.carbs_g);
  assert (updated.fat_g = meal.fat_g);
  assert (updated.confidence = meal.confidence);
  assert (updated.estimate_source = meal.estimate_source);
  assert (updated.notes = meal.notes)

let () =
  Alcotest.run "properties"
    [
      ( "time",
        [
          Alcotest.test_case "day_bounds is a 24h window containing the original time" `Quick
            day_bounds_is_a_24_hour_window_containing_the_original_time;
          Alcotest.test_case "parse_date round-trips through to_date_string" `Quick
            parse_date_round_trips_through_to_date_string;
        ] );
      ( "meal patch",
        [
          Alcotest.test_case "meal patch touches only the included fields" `Quick
            meal_patch_touches_only_the_included_fields;
        ] );
    ]
