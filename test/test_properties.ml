open Hegel

let daily_series_ok zone range ~meals ~weigh_ins =
  Result.get_ok (Summary.daily_series zone range ~meals ~weigh_ins)

(* Years within [1000, 9999] fit Date.to_string's %04d-%02d-%02d shape and
   Date.of_string's strict "YYYY-MM-DD" check exactly, so bounding the
   generators here avoids exercising unrelated formatting edge cases.  The top
   year is left out so that adding up to a range's length of days stays within
   Ptime's calendar. *)
let year_gen = integers ~min_value:1000 ~max_value:9998 ()
let month_gen = integers ~min_value:1 ~max_value:12 ()
let day_gen = integers ~min_value:1 ~max_value:28 ()

let draw_date tc =
  let text =
    Printf.sprintf "%04d-%02d-%02d" (draw tc year_gen) (draw tc month_gen)
      (draw tc day_gen)
  in
  Result.get_ok (Date.of_string text)

(* Every supported IANA zone plus fixed offsets across the full +-14h range. *)
let draw_zone tc =
  let names = Time.supported_zone_names in
  let index =
    draw tc (integers ~min_value:0 ~max_value:(List.length names) ())
  in
  if index < List.length names then
    Result.get_ok (Time.parse_zone (List.nth names index))
  else
    let minutes =
      draw tc (integers ~min_value:(-14 * 60) ~max_value:(14 * 60) ())
    in
    let sign = if minutes < 0 then '-' else '+' in
    Result.get_ok
      (Time.parse_zone
         (Printf.sprintf "%c%02d:%02d" sign
            (abs minutes / 60)
            (abs minutes mod 60)))

let draw_instant tc =
  let midnight = Date.midnight_utc (draw_date tc) in
  let seconds =
    draw tc (integers ~min_value:0 ~max_value:(Time.seconds_per_day - 1) ())
  in
  Option.get (Ptime.add_span midnight (Ptime.Span.of_int_s seconds))

let add_seconds time seconds =
  Option.get (Ptime.add_span time (Ptime.Span.of_int_s seconds))

let%hegel_test date_round_trips_through_its_string tc =
  let date = draw_date tc in
  match Date.of_string (Date.to_string date) with
  | Ok parsed -> assert (Date.equal parsed date)
  | Error _ -> assert false

let%hegel_test local_day_bounds_contain_exactly_the_instants_on_that_local_date
    tc =
  let zone = draw_zone tc in
  let instant = draw_instant tc in
  let date = Time.local_date zone instant in
  let start, stop = Time.local_day_bounds zone date in
  assert (Ptime.compare start instant <= 0 && Ptime.compare instant stop < 0);
  assert (Date.equal (Time.local_date zone start) date);
  assert (Date.equal (Time.local_date zone (add_seconds stop (-1))) date);
  assert (Date.equal (Time.local_date zone stop) (Date.add_days date 1))

let%hegel_test local_days_are_23_24_or_25_hours_and_tile_the_timeline tc =
  let zone = draw_zone tc in
  let date = draw_date tc in
  let start, stop = Time.local_day_bounds zone date in
  let hours = Ptime.Span.to_float_s (Ptime.diff stop start) /. 3600.0 in
  assert (List.exists (Float.equal hours) [ 23.0; 24.0; 25.0 ]);
  let next_start, _ = Time.local_day_bounds zone (Date.add_days date 1) in
  assert (Ptime.equal stop next_start)

let%hegel_test date_range_lists_consecutive_dates_from_first_to_last tc =
  let first = draw_date tc in
  let length =
    draw tc (integers ~min_value:1 ~max_value:Date_range.max_day_count ())
  in
  let last = Date.add_days first (length - 1) in
  let range = Result.get_ok (Date_range.make ~first ~last) in
  let dates = Date_range.dates range in
  assert (List.length dates = length && Date_range.day_count range = length);
  List.iteri
    (fun index date -> assert (Date.equal date (Date.add_days first index)))
    dates;
  assert (
    Result.is_error
      (Date_range.make ~first
         ~last:(Date.add_days first Date_range.max_day_count)));
  assert (
    length = 1 || Result.is_error (Date_range.make ~first:last ~last:first))

let meal_at eaten_at : Meal.t =
  Result.get_ok
    (Meal.of_storage ~id:(Meal_id.fresh ()) ~user_id:(User_id.fresh ())
       ~eaten_at ~description:"m" ~calories_kcal:100 ~protein_g:1.0
       ~carbs_g:None ~fat_g:None ~confidence:None ~estimate_source:None
       ~notes:None ~created_at:eaten_at ~updated_at:eaten_at ~deleted_at:None)

let%hegel_test daily_series_keeps_every_in_range_meal_exactly_once tc =
  let zone = draw_zone tc in
  let first = draw_date tc in
  let range =
    Result.get_ok
      (Date_range.make ~first
         ~last:
           (Date.add_days first
              (draw tc (integers ~min_value:0 ~max_value:5 ()))))
  in
  let start, _ = Date_range.utc_bounds zone range in
  (* Spread meals from a day before the range to a day after it. *)
  let meals =
    List.init
      (draw tc (integers ~min_value:0 ~max_value:20 ()))
      (fun _ ->
        meal_at
          (add_seconds start
             (draw tc
                (integers ~min_value:(-Time.seconds_per_day)
                   ~max_value:(8 * Time.seconds_per_day) ()))))
  in
  let in_range =
    List.filter
      (fun (meal : Meal.t) ->
        Date_range.mem range (Time.local_date zone meal.eaten_at))
      meals
  in
  let series = daily_series_ok zone range ~meals ~weigh_ins:[] in
  let counted =
    List.fold_left
      (fun sum (day : Summary.day) ->
        sum
        + Option.fold ~none:0
            ~some:(fun (t : Meal.totals) -> t.meal_count)
            day.meals)
      0 series
  in
  assert (List.length series = Date_range.day_count range);
  assert (counted = List.length in_range)

(* Generalizes the hand-picked "patch one field" examples in test_meal_store.ml:
   for any subset of description/calories_kcal/protein_g included in a patch,
   update_meal must change exactly those fields and leave the rest as they were. *)
let%hegel_test meal_patch_touches_only_the_included_fields tc =
  Test_support.with_store (fun store user ->
      let meal = Test_support.create_meal store user in
      let include_description = draw tc (booleans ()) in
      let include_calories = draw tc (booleans ()) in
      let include_protein = draw tc (booleans ()) in
      let new_description = "item " ^ draw tc (text ()) in
      let new_calories = draw tc (integers ~min_value:0 ()) in
      let new_protein =
        draw tc (floats ~min_value:0.0 ~allow_infinity:false ())
      in
      let patch =
        Meal.
          {
            empty_patch with
            description =
              (if include_description then Some new_description else None);
            calories_kcal =
              (if include_calories then Some new_calories else None);
            protein_g = (if include_protein then Some new_protein else None);
          }
      in
      let updated =
        Result.get_ok (Test_support.update_meal_raw store ~user meal.id patch)
      in
      assert (
        updated.description
        = if include_description then new_description else meal.description);
      assert (
        updated.calories_kcal
        = if include_calories then new_calories else meal.calories_kcal);
      assert (
        updated.protein_g
        = if include_protein then new_protein else meal.protein_g);
      (* Fields never included in this patch must never move, no matter what else did. *)
      assert (Option.equal Float.equal updated.carbs_g meal.carbs_g);
      assert (Option.equal Float.equal updated.fat_g meal.fat_g);
      assert (Option.equal Float.equal updated.confidence meal.confidence);
      assert (
        Option.equal String.equal updated.estimate_source meal.estimate_source);
      assert (Option.equal String.equal updated.notes meal.notes))

let () =
  Alcotest.run "properties"
    [
      ( "time",
        [
          Alcotest.test_case "date round-trips through its string" `Quick
            date_round_trips_through_its_string;
          Alcotest.test_case
            "local day bounds contain exactly the instants on that local date"
            `Quick
            local_day_bounds_contain_exactly_the_instants_on_that_local_date;
          Alcotest.test_case
            "local days are 23, 24 or 25 hours and tile the timeline" `Quick
            local_days_are_23_24_or_25_hours_and_tile_the_timeline;
          Alcotest.test_case
            "date range lists consecutive dates from first to last" `Quick
            date_range_lists_consecutive_dates_from_first_to_last;
          Alcotest.test_case
            "daily series keeps every in-range meal exactly once" `Quick
            daily_series_keeps_every_in_range_meal_exactly_once;
        ] );
      ( "meal patch",
        [
          Alcotest.test_case "meal patch touches only the included fields"
            `Quick meal_patch_touches_only_the_included_fields;
        ] );
    ]
