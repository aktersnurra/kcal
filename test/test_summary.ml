let time value = Result.get_ok (Time.parse_offset_datetime value)
let zone value = Result.get_ok (Time.parse_zone value)
let date value = Result.get_ok (Date.of_string value)
let stockholm = zone "Europe/Stockholm"

let meal ?(protein_g = 10.0) at calories_kcal : Meal.t =
  let at = time at in
  { id = Meal_id.fresh (); user_id = User_id.fresh (); eaten_at = at; description = "m";
    calories_kcal; protein_g; carbs_g = None; fat_g = None; confidence = None;
    estimate_source = None; notes = None; created_at = at; updated_at = at; deleted_at = None }

let weigh_in at weight_kg : Weigh_in.t =
  let at = time at in
  { id = Weigh_in_id.fresh (); user_id = User_id.fresh (); measured_at = at; weight_kg;
    source = Weigh_in.Manual; external_id = None; created_at = at; updated_at = at; deleted_at = None }

let range first last = Result.get_ok (Date_range.make ~first:(date first) ~last:(date last))
let check_float msg expected actual = Alcotest.(check (float 1e-6)) msg expected actual
let utc_string = Time.to_utc_string

let test_zone_offsets () =
  Alcotest.(check int) "winter" 3600 (Time.utc_offset_s_at stockholm (time "2026-01-15T12:00:00Z"));
  Alcotest.(check int) "summer" 7200 (Time.utc_offset_s_at stockholm (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int) "before spring switch" 3600 (Time.utc_offset_s_at stockholm (time "2026-03-29T00:59:59Z"));
  Alcotest.(check int) "after spring switch" 7200 (Time.utc_offset_s_at stockholm (time "2026-03-29T01:00:00Z"));
  Alcotest.(check int) "before autumn switch" 7200 (Time.utc_offset_s_at stockholm (time "2026-10-25T00:59:59Z"));
  Alcotest.(check int) "after autumn switch" 3600 (Time.utc_offset_s_at stockholm (time "2026-10-25T01:00:00Z"));
  Alcotest.(check int) "fixed" 19800 (Time.utc_offset_s_at (zone "+05:30") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int) "negative fixed" (-18000) (Time.utc_offset_s_at (zone "-05:00") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int) "Z is utc" 0 (Time.utc_offset_s_at (zone "Z") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check string) "name is kept" "Europe/Stockholm" (Time.zone_name stockholm);
  Alcotest.(check bool) "unknown rejected" true
    (match Time.parse_zone "Mars/Olympus" with Error (Error.Unsupported_timezone "Mars/Olympus") -> true | _ -> false);
  Alcotest.(check bool) "huge offset rejected" true (Result.is_error (Time.parse_zone "+15:00"));
  Alcotest.(check bool) "unknown local offset rejected" true (Result.is_error (Time.parse_zone "-00:00"))

let test_local_day_bounds () =
  let bounds zone value = let start, stop = Time.local_day_bounds zone (date value) in (utc_string start, utc_string stop) in
  Alcotest.(check (pair string string)) "summer day" ("2026-07-14T22:00:00Z", "2026-07-15T22:00:00Z") (bounds stockholm "2026-07-15");
  Alcotest.(check (pair string string)) "25 hour day" ("2026-10-24T22:00:00Z", "2026-10-25T23:00:00Z") (bounds stockholm "2026-10-25");
  Alcotest.(check (pair string string)) "23 hour day" ("2026-03-28T23:00:00Z", "2026-03-29T22:00:00Z") (bounds stockholm "2026-03-29");
  Alcotest.(check (pair string string)) "negative offset" ("2026-07-15T05:00:00Z", "2026-07-16T05:00:00Z") (bounds (zone "-05:00") "2026-07-15")

let test_date_parsing () =
  Alcotest.(check string) "round trip" "2026-02-28" (Date.to_string (date "2026-02-28"));
  Alcotest.(check bool) "malformed named" true (match Date.of_string "not-a-date" with Error (Error.Invalid_date "not-a-date") -> true | _ -> false);
  Alcotest.(check bool) "impossible day rejected" true (Result.is_error (Date.of_string "2026-02-30"))

let test_date_range () =
  Alcotest.(check (list string)) "inclusive" [ "2026-02-27"; "2026-02-28"; "2026-03-01" ]
    (List.map Date.to_string (Date_range.dates (range "2026-02-27" "2026-03-01")));
  Alcotest.(check bool) "reversed named" true
    (match Date_range.make ~first:(date "2026-03-02") ~last:(date "2026-03-01") with
     | Error (Error.Reversed_date_range { from_date = "2026-03-02"; to_date = "2026-03-01" }) -> true
     | _ -> false);
  Alcotest.(check bool) "too long named" true
    (match Date_range.make ~first:(date "2025-01-01") ~last:(date "2026-12-31") with
     | Error (Error.Date_range_too_long { day_count = 730; max_day_count = 366 }) -> true
     | _ -> false);
  let start, stop = Date_range.utc_bounds stockholm (range "2026-07-14" "2026-07-15") in
  Alcotest.(check (pair string string)) "exact utc bounds" ("2026-07-13T22:00:00Z", "2026-07-15T22:00:00Z") (utc_string start, utc_string stop)

let calories (day : Summary.day) = Option.map (fun (totals : Meal.totals) -> totals.calories_kcal) day.meals

(* 23:30 UTC on the 14th is 01:30 on the 15th in Stockholm summer time. *)
let test_late_meal_lands_on_the_local_day () =
  let meals = [ meal "2026-07-14T23:30:00Z" 400; meal "2026-07-15T10:00:00Z" 600 ] in
  let series zone = Summary.daily_series zone (range "2026-07-14" "2026-07-15") ~meals ~weigh_ins:[] in
  Alcotest.(check (list (option int))) "utc" [ Some 400; Some 600 ] (List.map calories (series Time.utc));
  Alcotest.(check (list (option int))) "stockholm" [ None; Some 1000 ] (List.map calories (series stockholm))

let test_series_carries_daily_weight () =
  let weigh_ins = [ weigh_in "2026-07-14T07:00:00Z" 80.0; weigh_in "2026-07-14T21:00:00Z" 81.0; weigh_in "2026-07-16T07:00:00Z" 79.0 ] in
  let series = Summary.daily_series Time.utc (range "2026-07-14" "2026-07-15") ~meals:[] ~weigh_ins in
  let weights = List.map (fun (day : Summary.day) -> Option.map (fun (w : Summary.daily_weight) -> (w.weigh_in_count, w.mean_weight_kg)) day.weight) series in
  Alcotest.(check (list (option (pair int (float 1e-6))))) "mean per day, out of range ignored" [ Some (2, 80.5); None ] weights

let calorie_target value = Option.get (Summary.Calorie_target.of_kcal value)
let protein_target value = Option.get (Summary.Protein_target.of_g value)

let test_intake_ignores_unlogged_days_and_counts_targets () =
  let meals = [ meal ~protein_g:150.0 "2026-07-14T10:00:00Z" 2000; meal ~protein_g:100.0 "2026-07-16T10:00:00Z" 2400 ] in
  let summary =
    Summary.summarise ~calorie_target:(calorie_target 2200) ~protein_target:(protein_target 120.0)
      Time.utc (range "2026-07-14" "2026-07-16") ~meals ~weigh_ins:[] in
  Alcotest.(check int) "days" 3 summary.day_count;
  let logged = Option.get summary.logged_intake in
  Alcotest.(check int) "logged" 2 logged.logged_day_count;
  check_float "avg kcal" 2200.0 logged.avg_calories_kcal;
  check_float "avg protein" 125.0 logged.avg_protein_g;
  Alcotest.(check (option int)) "under" (Some 1) (Option.map (fun (r : _ Summary.target_result) -> r.days_met) summary.calorie_target);
  Alcotest.(check (option int)) "protein" (Some 1) (Option.map (fun (r : _ Summary.target_result) -> r.days_met) summary.protein_target);
  Alcotest.(check bool) "nonpositive targets rejected" true
    (Option.is_none (Summary.Calorie_target.of_kcal 0) && Option.is_none (Summary.Protein_target.of_g (-1.0)));
  let empty = Summary.summarise Time.utc (range "2026-07-14" "2026-07-14") ~meals:[] ~weigh_ins:[] in
  Alcotest.(check bool) "no intake without meals" true (Option.is_none empty.logged_intake);
  Alcotest.(check bool) "maintenance says why" true (match empty.implied_maintenance with Summary.Unavailable Summary.No_intake_logged -> true | _ -> false)

let test_weight_trend_and_maintenance () =
  let weigh_ins = [ weigh_in "2026-07-01T07:00:00Z" 80.0; weigh_in "2026-07-08T07:00:00Z" 79.5; weigh_in "2026-07-15T07:00:00Z" 79.0 ] in
  let meals = [ meal "2026-07-02T10:00:00Z" 2000 ] in
  let summary = Summary.summarise Time.utc (range "2026-07-01" "2026-07-15") ~meals ~weigh_ins in
  (match summary.weight_trend with
  | Some (Summary.Trend trend) ->
      Alcotest.(check int) "count" 3 trend.weigh_in_count;
      check_float "change" (-1.0) trend.change_kg;
      check_float "span" 14.0 trend.span_days;
      check_float "slope" (-0.5) trend.slope_kg_per_week
  | _ -> Alcotest.fail "expected a trend");
  (match summary.implied_maintenance with
  | Summary.Estimate { kcal } -> check_float "maintenance" (2000.0 +. (0.5 /. 7.0 *. 7700.0)) kcal
  | Summary.Unavailable _ -> Alcotest.fail "expected an estimate");
  let short = Summary.summarise Time.utc (range "2026-07-01" "2026-07-15") ~meals
    ~weigh_ins:[ weigh_in "2026-07-01T07:00:00Z" 80.0; weigh_in "2026-07-03T07:00:00Z" 79.9 ] in
  (match short.implied_maintenance with
  | Summary.Unavailable (Summary.Span_too_short { span_days; required_days }) ->
      check_float "short span" 2.0 span_days;
      check_float "required span" 7.0 required_days
  | _ -> Alcotest.fail "expected span_too_short");
  let single = Summary.summarise Time.utc (range "2026-07-01" "2026-07-15") ~meals ~weigh_ins:[ weigh_in "2026-07-03T07:00:00Z" 79.9 ] in
  Alcotest.(check bool) "one reading is not a trend" true
    (match single.weight_trend with Some (Summary.Single_reading { weight_kg; _ }) -> Float.equal weight_kg 79.9 | _ -> false);
  Alcotest.(check bool) "single reading has no maintenance" true
    (match single.implied_maintenance with Summary.Unavailable Summary.No_weight_trend -> true | _ -> false);
  let outside = Summary.summarise Time.utc (range "2026-07-01" "2026-07-15") ~meals ~weigh_ins:[ weigh_in "2026-07-20T07:00:00Z" 70.0 ] in
  Alcotest.(check bool) "weigh-ins outside the range are ignored" true (Option.is_none outside.weight_trend)

let () = Alcotest.run "summary" [
  ("time", [
    Alcotest.test_case "zone offsets" `Quick test_zone_offsets;
    Alcotest.test_case "local day bounds" `Quick test_local_day_bounds;
    Alcotest.test_case "date parsing" `Quick test_date_parsing;
    Alcotest.test_case "date range" `Quick test_date_range;
  ]);
  ("summary", [
    Alcotest.test_case "late meal lands on the local day" `Quick test_late_meal_lands_on_the_local_day;
    Alcotest.test_case "series carries daily weight" `Quick test_series_carries_daily_weight;
    Alcotest.test_case "intake ignores unlogged days and counts targets" `Quick test_intake_ignores_unlogged_days_and_counts_targets;
    Alcotest.test_case "weight trend and maintenance" `Quick test_weight_trend_and_maintenance;
  ]);
]
