let daily_series_ok zone range ~meals ~weigh_ins =
  Result.get_ok (Summary.daily_series zone range ~meals ~weigh_ins)

let summarise_ok ?calorie_target ?protein_target zone range ~meals ~weigh_ins =
  Result.get_ok
    (Summary.summarise ?calorie_target ?protein_target zone range ~meals
       ~weigh_ins)

let time value = Result.get_ok (Time.parse_offset_datetime value)
let zone value = Result.get_ok (Time.parse_zone value)
let date value = Result.get_ok (Date.of_string value)
let stockholm = zone "Europe/Stockholm"

let meal ?(protein_g = 10.0) ?carbs_g ?fat_g at calories_kcal : Meal.t =
  let at = time at in
  Result.get_ok
    (Meal.of_storage ~id:(Meal_id.fresh ()) ~user_id:(User_id.fresh ())
       ~eaten_at:at ~description:"m" ~calories_kcal ~protein_g ~carbs_g ~fat_g
       ~confidence:None ~estimate_source:None ~notes:None ~created_at:at
       ~updated_at:at ~deleted_at:None)

let weigh_in at weight_kg : Weigh_in.t =
  let at = time at in
  Result.get_ok
    (Weigh_in.of_storage ~id:(Weigh_in_id.fresh ()) ~user_id:(User_id.fresh ())
       ~measured_at:at ~weight_kg ~source:Weigh_in.Manual ~external_id:None
       ~created_at:at ~updated_at:at ~deleted_at:None)

let range first last =
  Result.get_ok (Date_range.make ~first:(date first) ~last:(date last))

let check_float msg expected actual =
  Alcotest.(check (float 1e-6)) msg expected actual

let utc_string = Time.to_utc_string

let test_zone_offsets () =
  Alcotest.(check int)
    "winter" 3600
    (Time.utc_offset_s_at stockholm (time "2026-01-15T12:00:00Z"));
  Alcotest.(check int)
    "summer" 7200
    (Time.utc_offset_s_at stockholm (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int)
    "before spring switch" 3600
    (Time.utc_offset_s_at stockholm (time "2026-03-29T00:59:59Z"));
  Alcotest.(check int)
    "after spring switch" 7200
    (Time.utc_offset_s_at stockholm (time "2026-03-29T01:00:00Z"));
  Alcotest.(check int)
    "before autumn switch" 7200
    (Time.utc_offset_s_at stockholm (time "2026-10-25T00:59:59Z"));
  Alcotest.(check int)
    "after autumn switch" 3600
    (Time.utc_offset_s_at stockholm (time "2026-10-25T01:00:00Z"));
  Alcotest.(check int)
    "fixed" 19800
    (Time.utc_offset_s_at (zone "+05:30") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int)
    "negative fixed" (-18000)
    (Time.utc_offset_s_at (zone "-05:00") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int)
    "Z is utc" 0
    (Time.utc_offset_s_at (zone "Z") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check string)
    "name is kept" "Europe/Stockholm" (Time.zone_name stockholm);
  Alcotest.(check bool)
    "unknown rejected" true
    (match Time.parse_zone "Mars/Olympus" with
    | Error (Error.Unsupported_timezone "Mars/Olympus") -> true
    | _ -> false);
  Alcotest.(check bool)
    "huge offset rejected" true
    (Result.is_error (Time.parse_zone "+15:00"));
  Alcotest.(check bool)
    "unknown local offset rejected" true
    (Result.is_error (Time.parse_zone "-00:00"))

let test_local_day_bounds () =
  let bounds zone value =
    let start, stop = Result.get_ok (Time.local_day_bounds zone (date value)) in
    (utc_string start, utc_string stop)
  in
  Alcotest.(check (pair string string))
    "summer day"
    ("2026-07-14T22:00:00Z", "2026-07-15T22:00:00Z")
    (bounds stockholm "2026-07-15");
  Alcotest.(check (pair string string))
    "25 hour day"
    ("2026-10-24T22:00:00Z", "2026-10-25T23:00:00Z")
    (bounds stockholm "2026-10-25");
  Alcotest.(check (pair string string))
    "23 hour day"
    ("2026-03-28T23:00:00Z", "2026-03-29T22:00:00Z")
    (bounds stockholm "2026-03-29");
  Alcotest.(check (pair string string))
    "negative offset"
    ("2026-07-15T05:00:00Z", "2026-07-16T05:00:00Z")
    (bounds (zone "-05:00") "2026-07-15")

let test_date_parsing () =
  Alcotest.(check string)
    "round trip" "2026-02-28"
    (Date.to_string (date "2026-02-28"));
  Alcotest.(check bool)
    "malformed named" true
    (match Date.of_string "not-a-date" with
    | Error (Error.Invalid_date "not-a-date") -> true
    | _ -> false);
  Alcotest.(check bool)
    "impossible day rejected" true
    (Result.is_error (Date.of_string "2026-02-30"))

let test_date_range () =
  Alcotest.(check (list string))
    "inclusive"
    [ "2026-02-27"; "2026-02-28"; "2026-03-01" ]
    (List.map Date.to_string
       (Date_range.dates (range "2026-02-27" "2026-03-01")));
  Alcotest.(check bool)
    "reversed named" true
    (match
       Date_range.make ~first:(date "2026-03-02") ~last:(date "2026-03-01")
     with
    | Error
        (Error.Reversed_date_range
           { from_date = "2026-03-02"; to_date = "2026-03-01" }) ->
        true
    | _ -> false);
  Alcotest.(check bool)
    "too long named" true
    (match
       Date_range.make ~first:(date "2025-01-01") ~last:(date "2026-12-31")
     with
    | Error (Error.Date_range_too_long { day_count = 730; max_day_count = 366 })
      ->
        true
    | _ -> false);
  let start, stop =
    Result.get_ok
      (Date_range.utc_bounds stockholm (range "2026-07-14" "2026-07-15"))
  in
  Alcotest.(check (pair string string))
    "exact utc bounds"
    ("2026-07-13T22:00:00Z", "2026-07-15T22:00:00Z")
    (utc_string start, utc_string stop)

let calories (day : Summary.day) =
  Option.map (fun (totals : Meal.totals) -> totals.calories_kcal) day.meals

(* 23:30 UTC on the 14th is 01:30 on the 15th in Stockholm summer time. *)
let test_late_meal_lands_on_the_local_day () =
  let meals =
    [ meal "2026-07-14T23:30:00Z" 400; meal "2026-07-15T10:00:00Z" 600 ]
  in
  let series zone =
    daily_series_ok zone (range "2026-07-14" "2026-07-15") ~meals ~weigh_ins:[]
  in
  Alcotest.(check (list (option int)))
    "utc" [ Some 400; Some 600 ]
    (List.map calories (series Time.utc));
  Alcotest.(check (list (option int)))
    "stockholm" [ None; Some 1000 ]
    (List.map calories (series stockholm))

let test_series_carries_daily_weight () =
  let weigh_ins =
    [
      weigh_in "2026-07-14T07:00:00Z" 80.0;
      weigh_in "2026-07-14T21:00:00Z" 81.0;
      weigh_in "2026-07-16T07:00:00Z" 79.0;
    ]
  in
  let series =
    daily_series_ok Time.utc
      (range "2026-07-14" "2026-07-15")
      ~meals:[] ~weigh_ins
  in
  let weights =
    List.map
      (fun (day : Summary.day) ->
        Option.map
          (fun (w : Summary.daily_weight) ->
            (w.weigh_in_count, w.mean_weight_kg))
          day.weight)
      series
  in
  Alcotest.(check (list (option (pair int (float 1e-6)))))
    "mean per day, out of range ignored"
    [ Some (2, 80.5); None ]
    weights

let calorie_target value = Result.get_ok (Summary.Calorie_target.of_kcal value)
let protein_target value = Result.get_ok (Summary.Protein_target.of_g value)

let test_intake_ignores_unlogged_days_and_counts_targets () =
  let meals =
    [
      meal ~protein_g:150.0 "2026-07-14T10:00:00Z" 2000;
      meal ~protein_g:100.0 "2026-07-16T10:00:00Z" 2400;
    ]
  in
  let summary =
    summarise_ok ~calorie_target:(calorie_target 2200)
      ~protein_target:(protein_target 120.0) Time.utc
      (range "2026-07-14" "2026-07-16")
      ~meals ~weigh_ins:[]
  in
  Alcotest.(check int) "days" 3 summary.day_count;
  let logged = Option.get summary.logged_intake in
  Alcotest.(check int) "logged" 2 logged.logged_day_count;
  check_float "avg kcal" 2200.0 logged.avg_calories_kcal;
  check_float "avg protein" 125.0 logged.avg_protein_g;
  Alcotest.(check (option int))
    "under" (Some 1)
    (Option.map
       (fun (r : _ Summary.target_result) -> r.days_met)
       summary.calorie_target);
  Alcotest.(check (option int))
    "protein" (Some 1)
    (Option.map
       (fun (r : _ Summary.target_result) -> r.days_met)
       summary.protein_target);
  Alcotest.(check bool)
    "nonpositive targets rejected" true
    (Result.is_error (Summary.Calorie_target.of_kcal 0)
    && Result.is_error (Summary.Protein_target.of_g (-1.0)));
  let empty =
    summarise_ok Time.utc
      (range "2026-07-14" "2026-07-14")
      ~meals:[] ~weigh_ins:[]
  in
  Alcotest.(check bool)
    "no intake without meals" true
    (Option.is_none empty.logged_intake);
  Alcotest.(check bool)
    "maintenance says why" true
    (match empty.implied_maintenance with
    | Summary.Unavailable Summary.No_intake_logged -> true
    | _ -> false)

let test_weight_trend_and_maintenance () =
  let weigh_ins =
    [
      weigh_in "2026-07-01T07:00:00Z" 80.0;
      weigh_in "2026-07-08T07:00:00Z" 79.5;
      weigh_in "2026-07-15T07:00:00Z" 79.0;
    ]
  in
  let meals = [ meal "2026-07-02T10:00:00Z" 2000 ] in
  let summary =
    summarise_ok Time.utc (range "2026-07-01" "2026-07-15") ~meals ~weigh_ins
  in
  (match summary.weight_trend with
  | Some (Summary.Trend trend) ->
      Alcotest.(check int) "count" 3 trend.weigh_in_count;
      check_float "change" (-1.0) trend.change_kg;
      check_float "span" 14.0 trend.span_days;
      check_float "slope" (-0.5) trend.slope_kg_per_week
  | _ -> Alcotest.fail "expected a trend");
  (match summary.implied_maintenance with
  | Summary.Estimate { kcal } ->
      check_float "maintenance" (2000.0 +. (0.5 /. 7.0 *. 7700.0)) kcal
  | Summary.Unavailable _ -> Alcotest.fail "expected an estimate");
  let short =
    summarise_ok Time.utc
      (range "2026-07-01" "2026-07-15")
      ~meals
      ~weigh_ins:
        [
          weigh_in "2026-07-01T07:00:00Z" 80.0;
          weigh_in "2026-07-03T07:00:00Z" 79.9;
        ]
  in
  (match short.implied_maintenance with
  | Summary.Unavailable (Summary.Span_too_short { span_days; required_days }) ->
      check_float "short span" 2.0 span_days;
      check_float "required span" 7.0 required_days
  | _ -> Alcotest.fail "expected span_too_short");
  let single =
    summarise_ok Time.utc
      (range "2026-07-01" "2026-07-15")
      ~meals
      ~weigh_ins:[ weigh_in "2026-07-03T07:00:00Z" 79.9 ]
  in
  Alcotest.(check bool)
    "one reading is not a trend" true
    (match single.weight_trend with
    | Some (Summary.Single_reading { weight_kg; _ }) ->
        Float.equal weight_kg 79.9
    | _ -> false);
  Alcotest.(check bool)
    "single reading has no maintenance" true
    (match single.implied_maintenance with
    | Summary.Unavailable Summary.No_weight_trend -> true
    | _ -> false);
  let outside =
    summarise_ok Time.utc
      (range "2026-07-01" "2026-07-15")
      ~meals
      ~weigh_ins:[ weigh_in "2026-07-20T07:00:00Z" 70.0 ]
  in
  Alcotest.(check bool)
    "weigh-ins outside the range are ignored" true
    (Option.is_none outside.weight_trend)

let test_rejects_nonfinite_target () =
  Alcotest.(check bool)
    "infinite target rejected" true
    (Result.is_error (Summary.Protein_target.of_g infinity))

let test_calorie_total_does_not_wrap () =
  let meals =
    [ meal "2026-07-14T10:00:00Z" max_int; meal "2026-07-14T10:00:00Z" 1 ]
  in
  Alcotest.(check bool)
    "typed overflow" true
    (match
       Summary.daily_series Time.utc
         (range "2026-07-14" "2026-07-14")
         ~meals ~weigh_ins:[]
     with
    | Error (Error.Numeric_overflow Calories_kcal) -> true
    | _ -> false);
  Alcotest.(check bool)
    "summary propagates overflow" true
    (match
       Summary.summarise Time.utc
         (range "2026-07-14" "2026-07-14")
         ~meals ~weigh_ins:[]
     with
    | Error (Error.Numeric_overflow Calories_kcal) -> true
    | _ -> false)

let%hegel_test checked_calorie_totals tc =
  let a = Hegel.draw tc (Hegel.integers ~min_value:0 ()) in
  let b = Hegel.draw tc (Hegel.integers ~min_value:0 ()) in
  let expected = Int64.add (Int64.of_int a) (Int64.of_int b) in
  let meals =
    [ meal "2026-07-14T10:00:00Z" a; meal "2026-07-14T10:00:00Z" b ]
  in
  match
    Summary.daily_series Time.utc
      (range "2026-07-14" "2026-07-14")
      ~meals ~weigh_ins:[]
  with
  | Error (Error.Numeric_overflow Calories_kcal) ->
      assert (Int64.compare expected (Int64.of_int max_int) > 0)
  | Ok [ day ] ->
      assert (
        Option.equal Int64.equal (Some expected)
          (Option.map Int64.of_int (calories day)))
  | _ -> assert false

let test_checked_float_totals_and_stable_means () =
  let maximum = Float.max_float in
  let at = "2026-07-14T10:00:00Z" in
  let meals = [ meal ~protein_g:maximum at 0; meal ~protein_g:maximum at 0 ] in
  Alcotest.(check bool)
    "unrepresentable daily protein" true
    (match
       Summary.daily_series Time.utc
         (range "2026-07-14" "2026-07-14")
         ~meals ~weigh_ins:[]
     with
    | Error (Error.Unrepresentable_result Daily_meals) -> true
    | _ -> false);
  let meals =
    [
      meal ~protein_g:maximum at 0;
      meal ~protein_g:maximum "2026-07-15T10:00:00Z" 0;
    ]
  in
  let summary =
    summarise_ok Time.utc (range "2026-07-14" "2026-07-15") ~meals ~weigh_ins:[]
  in
  Alcotest.(check bool)
    "representable mean avoids sum overflow" true
    (Float.equal maximum (Option.get summary.logged_intake).avg_protein_g);
  let weigh_ins = [ weigh_in at maximum; weigh_in at maximum ] in
  let series =
    daily_series_ok Time.utc
      (range "2026-07-14" "2026-07-14")
      ~meals:[] ~weigh_ins
  in
  Alcotest.(check bool)
    "mean weight avoids sum overflow" true
    (match series with
    | [ day ] -> Float.equal maximum (Option.get day.weight).mean_weight_kg
    | _ -> false);
  let weigh_ins =
    [ weigh_in at maximum; weigh_in "2026-07-15T10:00:00Z" 1.0 ]
  in
  Alcotest.(check bool)
    "unrepresentable regression is explicit" true
    (match
       Summary.summarise Time.utc
         (range "2026-07-14" "2026-07-15")
         ~meals:[] ~weigh_ins
     with
    | Error (Error.Unrepresentable_result Weight_regression) -> true
    | _ -> false);
  let weigh_ins =
    [ weigh_in at maximum; weigh_in "2026-07-15T10:00:00Z" maximum ]
  in
  let summary =
    summarise_ok Time.utc (range "2026-07-14" "2026-07-15") ~meals:[] ~weigh_ins
  in
  Alcotest.(check bool)
    "constant extreme weights have zero slope" true
    (match summary.weight_trend with
    | Some (Summary.Trend trend) -> Float.equal 0.0 trend.slope_kg_per_week
    | _ -> false)

let%hegel_test checked_protein_totals tc =
  let a =
    Hegel.draw tc (Hegel.floats ~min_value:0.0 ~allow_infinity:false ())
  in
  let b =
    Hegel.draw tc (Hegel.floats ~min_value:0.0 ~allow_infinity:false ())
  in
  let at = "2026-07-14T10:00:00Z" in
  let meals = [ meal ~protein_g:a at 0; meal ~protein_g:b at 0 ] in
  let expected = a +. b in
  match
    Summary.daily_series Time.utc
      (range "2026-07-14" "2026-07-14")
      ~meals ~weigh_ins:[]
  with
  | Error (Error.Unrepresentable_result Daily_meals) ->
      assert (not (Float.is_finite expected))
  | Ok [ day ] ->
      assert (
        Float.is_finite expected
        && Float.equal expected (Option.get day.meals).protein_g)
  | _ -> assert false

let%hegel_test checked_optional_macro_totals_preserve_unknown tc =
  let a =
    Hegel.draw tc
      (Hegel.optional (Hegel.floats ~min_value:0.0 ~allow_infinity:false ()))
  in
  let b =
    Hegel.draw tc
      (Hegel.optional (Hegel.floats ~min_value:0.0 ~allow_infinity:false ()))
  in
  let at = "2026-07-14T10:00:00Z" in
  let meals =
    [ meal ?carbs_g:a ?fat_g:b at 0; meal ?carbs_g:b ?fat_g:a at 0 ]
  in
  let present = List.filter_map Fun.id [ a; b ] in
  let expected =
    match present with
    | [] -> None
    | values -> Some (List.fold_left ( +. ) 0.0 values)
  in
  match
    Summary.daily_series Time.utc
      (range "2026-07-14" "2026-07-14")
      ~meals ~weigh_ins:[]
  with
  | Error (Error.Unrepresentable_result Daily_meals) ->
      assert (
        Option.fold ~none:false
          ~some:(fun n -> not (Float.is_finite n))
          expected)
  | Ok [ day ] ->
      let totals = Option.get day.meals in
      assert (Option.equal Float.equal expected totals.carbs_g);
      assert (Option.equal Float.equal expected totals.fat_g)
  | _ -> assert false

module Calendar_properties = struct
  module Domain_error = Error
  open Base
  module Error = Domain_error
  module Calendar = Calendar_test_support

  let ok = function
    | Ok value -> value
    | Error error -> failwith (Error.to_string error)

  let check_bounds zone date =
    match
      (Time.local_day_bounds zone date, Calendar.expected_bounds zone date)
    with
    | Ok (start, stop), Some (expected_start, expected_stop) ->
        assert (
          Ptime.equal start expected_start && Ptime.equal stop expected_stop)
    | Error (Error.Calendar_day_bounds _), None -> ()
    | _ -> assert false

  let%hegel_test
      local_day_bounds_contain_exactly_the_instants_on_that_local_date tc =
    let zone = Calendar.draw_zone tc in
    let instant = Calendar.draw_instant tc in
    let offset = Time.utc_offset_s_at zone instant in
    let _, (_, actual_offset) =
      Ptime.to_date_time ~tz_offset_s:offset instant
    in
    match Time.local_date zone instant with
    | Error (Error.Calendar_local_date _) -> assert (actual_offset <> offset)
    | Error _ -> assert false
    | Ok date -> (
        assert (actual_offset = offset);
        check_bounds zone date;
        match Time.local_day_bounds zone date with
        | Error _ ->
            assert (Option.is_none (Calendar.expected_bounds zone date))
        | Ok (start, stop) ->
            assert (
              Ptime.compare start instant <= 0 && Ptime.compare instant stop < 0);
            assert (Date.equal (ok (Time.local_date zone start)) date);
            assert (
              Date.equal
                (ok
                   (Time.local_date zone
                      (Option.value_exn (Calendar.add_seconds stop (-1)))))
                date);
            assert (
              Date.equal
                (ok (Time.local_date zone stop))
                (ok (Date.add_days date 1))))

  let%hegel_test local_days_are_23_24_or_25_hours_and_tile_the_timeline tc =
    let zone = Calendar.draw_zone tc in
    let date = Calendar.draw_date tc in
    check_bounds zone date;
    match Time.local_day_bounds zone date with
    | Error _ ->
        () (* check_bounds independently verified unrepresentability. *)
    | Ok (start, stop) -> (
        let hours = Ptime.Span.to_float_s (Ptime.diff stop start) /. 3600.0 in
        assert (List.exists [ 23.0; 24.0; 25.0 ] ~f:(Float.equal hours));
        let next = ok (Date.add_days date 1) in
        match Time.local_day_bounds zone next with
        | Ok (next_start, _) -> assert (Ptime.equal stop next_start)
        | Error _ ->
            assert (Option.is_none (Calendar.expected_bounds zone next)))

  let%hegel_test fixed_zone_bounds_match_offset_datetime_constructor tc =
    let date = Calendar.draw_date tc in
    let minutes =
      Hegel.draw tc (Hegel.integers ~min_value:(-840) ~max_value:840 ())
    in
    let zone =
      ok
        (Time.parse_zone
           (Printf.sprintf "%c%02d:%02d"
              (if minutes < 0 then '-' else '+')
              (Int.abs minutes / 60)
              (Int.abs minutes % 60)))
    in
    let midnight ymd = Ptime.of_date_time (ymd, ((0, 0, 0), minutes * 60)) in
    let expected =
      Option.bind (Calendar.next_ymd date) ~f:(fun next ->
          Option.both
            (midnight (Ptime.to_date (Date.midnight_utc date)))
            (midnight next))
    in
    match (Time.local_day_bounds zone date, expected) with
    | Ok (start, stop), Some (a, b) ->
        assert (Ptime.equal start a && Ptime.equal stop b)
    | Error (Error.Calendar_day_bounds _), None -> ()
    | _ -> assert false

  let%hegel_test local_date_conversion_never_accepts_utc_fallback tc =
    let instant = Calendar.draw_instant tc in
    let utc_offset_s = Hegel.draw tc (Hegel.integers ()) in
    let ymd, ((hour, minute, second), _) = Ptime.to_date_time instant in
    let seconds_of_day = (hour * 3600) + (minute * 60) + second in
    let shifted_seconds =
      Int64.(
        (of_int (Calendar.ordinal_ymd ymd) * 86400L)
        + of_int seconds_of_day + of_int utc_offset_s)
    in
    let representable =
      Int64.(
        shifted_seconds >= 0L
        && shifted_seconds < (of_int Calendar.last_ordinal + 1L) * 86400L)
    in
    match Date.of_instant ~utc_offset_s instant with
    | Ok date ->
        assert representable;
        assert (
          Int64.equal
            (Int64.of_int (Calendar.ordinal date))
            Int64.(shifted_seconds / 86400L))
    | Error (Error.Calendar_local_date _) -> assert (not representable)
    | Error _ -> assert false

  let%hegel_test range_bounds_cover_inclusive_dates tc =
    let first = Calendar.draw_date tc in
    let zone = Calendar.draw_zone tc in
    let available = Calendar.last_ordinal - Calendar.ordinal first in
    let last =
      ok
        (Date.add_days first
           (Hegel.draw tc
              (Hegel.integers ~min_value:0
                 ~max_value:(Int.min available (Date_range.max_day_count - 1))
                 ())))
    in
    let range = ok (Date_range.make ~first ~last) in
    match
      ( Date_range.utc_bounds zone range,
        Calendar.expected_bounds zone first,
        Calendar.expected_bounds zone last )
    with
    | Ok (start, stop), Some (a, _), Some (_, b) ->
        assert (Ptime.equal start a && Ptime.equal stop b)
    | Error (Error.Calendar_day_bounds _), a, b ->
        assert (Option.is_none a || Option.is_none b)
    | _ -> assert false

  let meal_at eaten_at =
    ok
      (Meal.of_storage ~id:(Meal_id.fresh ()) ~user_id:(User_id.fresh ())
         ~eaten_at ~description:"m" ~calories_kcal:100 ~protein_g:1.0
         ~carbs_g:None ~fat_g:None ~confidence:None ~estimate_source:None
         ~notes:None ~created_at:eaten_at ~updated_at:eaten_at ~deleted_at:None)

  let%hegel_test daily_series_keeps_every_in_range_meal_exactly_once tc =
    let zone = Calendar.draw_zone tc in
    let first = Calendar.draw_date tc in
    let available = Calendar.last_ordinal - Calendar.ordinal first in
    let last =
      ok
        (Date.add_days first
           (Hegel.draw tc
              (Hegel.integers ~min_value:0 ~max_value:(Int.min available 5) ())))
    in
    let range = ok (Date_range.make ~first ~last) in
    match Date_range.utc_bounds zone range with
    | Error _ ->
        assert (
          Result.is_error
            (Summary.daily_series zone range ~meals:[] ~weigh_ins:[]))
    | Ok (start, stop) ->
        (* Out-of-calendar instants are not representable input records. *)
        let meals =
          List.filter_map
            (List.init
               (Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:20 ()))
               ~f:Fn.id)
            ~f:(fun _ ->
              Option.map
                (Calendar.add_seconds start
                   (Hegel.draw tc
                      (Hegel.integers ~min_value:(-Time.seconds_per_day)
                         ~max_value:(8 * Time.seconds_per_day) ())))
                ~f:meal_at)
        in
        (* Independent interval membership, not the production local-date bucketer. *)
        let in_range =
          List.count meals ~f:(fun (meal : Meal.t) ->
              Ptime.compare start meal.eaten_at <= 0
              && Ptime.compare meal.eaten_at stop < 0)
        in
        let series =
          ok (Summary.daily_series zone range ~meals ~weigh_ins:[])
        in
        let counted =
          List.fold series ~init:0 ~f:(fun count (day : Summary.day) ->
              count
              + Option.value_map day.meals ~default:0
                  ~f:(fun (totals : Meal.totals) -> totals.meal_count))
        in
        assert (List.length series = Date_range.day_count range);
        assert (counted = in_range);
        List.iter series ~f:(fun (day : Summary.day) ->
            let expected =
              List.count meals ~f:(fun (meal : Meal.t) ->
                  let offset = Time.utc_offset_s_at zone meal.eaten_at in
                  let (year, month, day_of_month), (_, actual_offset) =
                    Ptime.to_date_time ~tz_offset_s:offset meal.eaten_at
                  in
                  actual_offset = offset
                  && String.equal
                       (Printf.sprintf "%04d-%02d-%02d" year month day_of_month)
                       (Date.to_string day.date))
            in
            assert (
              Option.value_map day.meals ~default:0 ~f:(fun (t : Meal.totals) ->
                  t.meal_count)
              = expected);
            assert (Bool.equal (Option.is_none day.meals) (expected = 0)))

  let test_calendar_endpoint_bounds () =
    List.iter
      (Time.supported_zone_names @ [ "-14:00"; "+14:00"; "-00:01"; "+00:01" ])
      ~f:(fun name ->
        let zone = ok (Time.parse_zone name) in
        List.iter
          [
            "0000-01-01"; "0000-01-02"; "0000-02-29"; "9999-12-30"; "9999-12-31";
          ] ~f:(fun text -> check_bounds zone (ok (Date.of_string text))));
    List.iter
      [ (Ptime.min, "-14:00"); (Ptime.max, "+14:00") ]
      ~f:(fun (instant, name) ->
        match Time.local_date (ok (Time.parse_zone name)) instant with
        | Error (Error.Calendar_local_date _) -> ()
        | _ -> assert false)

  let%hegel_test eu_transitions_match_gregorian_weekday_oracle tc =
    let year = Hegel.draw tc (Hegel.integers ~min_value:0 ~max_value:9999 ()) in
    let month = if Hegel.draw tc (Hegel.booleans ()) then 3 else 10 in
    let last = ok (Date.of_string (Printf.sprintf "%04d-%02d-31" year month)) in
    (* Gregorian 0000-01-01 was Saturday: Sunday has index 0 here. *)
    let day = 31 - ((Calendar.ordinal last + 6) % 7) in
    let date =
      ok (Date.of_string (Printf.sprintf "%04d-%02d-%02d" year month day))
    in
    let transition =
      Option.value_exn (Ptime.of_date_time ((year, month, day), ((1, 0, 0), 0)))
    in
    List.iter
      [
        ("Europe/London", 0);
        ("Europe/Stockholm", 3600);
        ("Europe/Helsinki", 7200);
      ]
      ~f:(fun (name, standard) ->
        let zone = ok (Time.parse_zone name) in
        let before = Option.value_exn (Calendar.add_seconds transition (-1)) in
        let expected_before, expected_after, hours =
          if month = 3 then (standard, standard + 3600, 23.)
          else (standard + 3600, standard, 25.)
        in
        assert (Time.utc_offset_s_at zone before = expected_before);
        assert (Time.utc_offset_s_at zone transition = expected_after);
        let start, stop = ok (Time.local_day_bounds zone date) in
        assert (
          Float.equal
            (Ptime.Span.to_float_s (Ptime.diff stop start))
            (hours *. 3600.)))

  let test_known_dst_boundaries () =
    (* Published EU transition dates; these fixtures do not calculate Sundays. *)
    List.iter
      [
        ("2024-03-31", "2024-10-27");
        ("2025-03-30", "2025-10-26");
        ("2026-03-29", "2026-10-25");
      ]
      ~f:(fun (spring, autumn) ->
        List.iter [ "Europe/London"; "Europe/Stockholm"; "Europe/Helsinki" ]
          ~f:(fun name ->
            let zone = ok (Time.parse_zone name) in
            List.iter
              [ (spring, 23.); (autumn, 25.) ]
              ~f:(fun (text, hours) ->
                let date = ok (Date.of_string text) in
                let start, stop = ok (Time.local_day_bounds zone date) in
                assert (
                  Float.equal
                    (Ptime.Span.to_float_s (Ptime.diff stop start))
                    (hours *. 3600.));
                let before, _ =
                  ok (Time.local_day_bounds zone (ok (Date.add_days date (-1))))
                in
                assert (
                  Float.equal
                    (Ptime.Span.to_float_s (Ptime.diff start before))
                    86400.))))
end

let () =
  Alcotest.run "summary"
    [
      ( "time",
        [
          Alcotest.test_case "day containment property" `Quick
            Calendar_properties
            .local_day_bounds_contain_exactly_the_instants_on_that_local_date;
          Alcotest.test_case "day length and tiling property" `Quick
            Calendar_properties
            .local_days_are_23_24_or_25_hours_and_tile_the_timeline;
          Alcotest.test_case "fixed offset oracle property" `Quick
            Calendar_properties
            .fixed_zone_bounds_match_offset_datetime_constructor;
          Alcotest.test_case "no local date fallback property" `Quick
            Calendar_properties.local_date_conversion_never_accepts_utc_fallback;
          Alcotest.test_case "inclusive range bounds property" `Quick
            Calendar_properties.range_bounds_cover_inclusive_dates;
          Alcotest.test_case "endpoint bounds" `Quick
            Calendar_properties.test_calendar_endpoint_bounds;
          Alcotest.test_case "Gregorian EU transition oracle property" `Quick
            Calendar_properties.eu_transitions_match_gregorian_weekday_oracle;
          Alcotest.test_case "known DST dates" `Quick
            Calendar_properties.test_known_dst_boundaries;
          Alcotest.test_case "zone offsets" `Quick test_zone_offsets;
          Alcotest.test_case "local day bounds" `Quick test_local_day_bounds;
          Alcotest.test_case "date parsing" `Quick test_date_parsing;
          Alcotest.test_case "date range" `Quick test_date_range;
        ] );
      ( "summary",
        [
          Alcotest.test_case "in-range meal conservation property" `Quick
            Calendar_properties
            .daily_series_keeps_every_in_range_meal_exactly_once;
          Alcotest.test_case "checked optional macros property" `Quick
            checked_optional_macro_totals_preserve_unknown;
          Alcotest.test_case "checked totals property" `Quick
            checked_calorie_totals;
          Alcotest.test_case "nonfinite target" `Quick
            test_rejects_nonfinite_target;
          Alcotest.test_case "checked float totals and stable means" `Quick
            test_checked_float_totals_and_stable_means;
          Alcotest.test_case "checked protein totals property" `Quick
            checked_protein_totals;
          Alcotest.test_case "calorie total does not wrap" `Quick
            test_calorie_total_does_not_wrap;
          Alcotest.test_case "late meal lands on the local day" `Quick
            test_late_meal_lands_on_the_local_day;
          Alcotest.test_case "series carries daily weight" `Quick
            test_series_carries_daily_weight;
          Alcotest.test_case "intake ignores unlogged days and counts targets"
            `Quick test_intake_ignores_unlogged_days_and_counts_targets;
          Alcotest.test_case "weight trend and maintenance" `Quick
            test_weight_trend_and_maintenance;
        ] );
    ]
