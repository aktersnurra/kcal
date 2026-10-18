let time value = Result.get_ok (Time.parse_offset_datetime value)
let zone value = Result.get_ok (Time.parse_zone value)
let stockholm = zone "Europe/Stockholm"

let meal ?(protein_g = 10.0) at calories_kcal : Meal.t =
  let at = time at in
  { id = Meal_id.fresh (); user_id = User_id.fresh (); eaten_at = at; description = "m";
    calories_kcal; protein_g; carbs_g = None; fat_g = None; confidence = None;
    estimate_source = None; notes = None; created_at = at; updated_at = at; deleted_at = None }

let weight at weight_kg : Weigh_in.t =
  let at = time at in
  { id = Weigh_in_id.fresh (); user_id = User_id.fresh (); measured_at = at; weight_kg;
    source = Weigh_in.Manual; external_id = None; created_at = at; updated_at = at; deleted_at = None }

let dates from_date to_date = Result.get_ok (Time.date_range ~from_date ~to_date)
let check_float msg expected actual = Alcotest.(check (float 1e-6)) msg expected actual

let test_zone_offsets () =
  Alcotest.(check int) "winter" 3600 (Time.offset_at stockholm (time "2026-01-15T12:00:00Z"));
  Alcotest.(check int) "summer" 7200 (Time.offset_at stockholm (time "2026-07-15T12:00:00Z"));
  Alcotest.(check int) "before spring switch" 3600 (Time.offset_at stockholm (time "2026-03-29T00:59:59Z"));
  Alcotest.(check int) "after spring switch" 7200 (Time.offset_at stockholm (time "2026-03-29T01:00:00Z"));
  Alcotest.(check int) "before autumn switch" 7200 (Time.offset_at stockholm (time "2026-10-25T00:59:59Z"));
  Alcotest.(check int) "after autumn switch" 3600 (Time.offset_at stockholm (time "2026-10-25T01:00:00Z"));
  Alcotest.(check int) "fixed" 19800 (Time.offset_at (zone "+05:30") (time "2026-07-15T12:00:00Z"));
  Alcotest.(check bool) "unknown rejected" true (Result.is_error (Time.parse_zone "Mars/Olympus"));
  Alcotest.(check bool) "huge offset rejected" true (Result.is_error (Time.parse_zone "+15:00"))

let test_local_day_bounds () =
  let day = Result.get_ok (Time.parse_date "2026-07-15") in
  let start, stop = Time.local_day_bounds stockholm day in
  Alcotest.(check string) "start" "2026-07-14T22:00:00Z" (Time.to_utc_string start);
  Alcotest.(check string) "stop" "2026-07-15T22:00:00Z" (Time.to_utc_string stop);
  let dst_day = Result.get_ok (Time.parse_date "2026-10-25") in
  let start, stop = Time.local_day_bounds stockholm dst_day in
  Alcotest.(check string) "25 hour day starts" "2026-10-24T22:00:00Z" (Time.to_utc_string start);
  Alcotest.(check string) "25 hour day ends" "2026-10-25T23:00:00Z" (Time.to_utc_string stop)

let test_date_range () =
  Alcotest.(check (list string)) "inclusive" [ "2026-02-27"; "2026-02-28"; "2026-03-01" ] (dates "2026-02-27" "2026-03-01");
  Alcotest.(check bool) "reversed rejected" true (Result.is_error (Time.date_range ~from_date:"2026-03-02" ~to_date:"2026-03-01"));
  Alcotest.(check bool) "too long rejected" true (Result.is_error (Time.date_range ~from_date:"2025-01-01" ~to_date:"2026-12-31"))

(* 23:30 UTC on the 14th is 01:30 on the 15th in Stockholm summer time. *)
let test_late_meal_lands_on_the_local_day () =
  let meals = [ meal "2026-07-14T23:30:00Z" 400; meal "2026-07-15T10:00:00Z" 600 ] in
  let series zone = Summary.daily_series ~zone ~dates:(dates "2026-07-14" "2026-07-15") meals in
  Alcotest.(check (list int)) "utc" [ 400; 600 ] (List.map (fun (d : Summary.day) -> d.calories_kcal) (series Time.utc));
  Alcotest.(check (list int)) "stockholm" [ 0; 1000 ] (List.map (fun (d : Summary.day) -> d.calories_kcal) (series stockholm))

let test_intake_ignores_unlogged_days_and_counts_targets () =
  let meals = [ meal ~protein_g:150.0 "2026-07-14T10:00:00Z" 2000; meal ~protein_g:100.0 "2026-07-16T10:00:00Z" 2400 ] in
  let series = Summary.daily_series ~zone:Time.utc ~dates:(dates "2026-07-14" "2026-07-16") meals in
  let intake = Summary.intake ~calorie_target:2200 ~protein_target:120.0 series in
  Alcotest.(check int) "days" 3 intake.days;
  Alcotest.(check int) "logged" 2 intake.logged_days;
  check_float "avg kcal" 2200.0 (Option.get intake.avg_calories_kcal);
  check_float "avg protein" 125.0 (Option.get intake.avg_protein_g);
  Alcotest.(check (option int)) "under" (Some 1) intake.days_at_or_under_calorie_target;
  Alcotest.(check (option int)) "protein" (Some 1) intake.days_at_or_over_protein_target;
  let empty = Summary.intake (Summary.daily_series ~zone:Time.utc ~dates:(dates "2026-07-14" "2026-07-14") []) in
  Alcotest.(check bool) "no average without meals" true (empty.avg_calories_kcal = None)

let test_weight_trend_and_maintenance () =
  let weights = [ weight "2026-07-01T07:00:00Z" 80.0; weight "2026-07-08T07:00:00Z" 79.5; weight "2026-07-15T07:00:00Z" 79.0 ] in
  let trend = Summary.weight_trend ~zone:Time.utc ~dates:(dates "2026-07-01" "2026-07-15") weights in
  let t = Option.get trend in
  Alcotest.(check int) "count" 3 t.weigh_in_count;
  check_float "change" (-1.0) t.change_kg;
  check_float "slope" (-0.5) (Option.get t.slope_kg_per_week);
  let series = Summary.daily_series ~zone:Time.utc ~dates:(dates "2026-07-01" "2026-07-15") [ meal "2026-07-02T10:00:00Z" 2000 ] in
  let intake = Summary.intake series in
  check_float "maintenance" (2000.0 +. (0.5 /. 7.0 *. 7700.0)) (Option.get (Summary.implied_maintenance_kcal intake trend));
  let short = Summary.weight_trend ~zone:Time.utc ~dates:(dates "2026-07-01" "2026-07-15") [ weight "2026-07-01T07:00:00Z" 80.0; weight "2026-07-03T07:00:00Z" 79.9 ] in
  Alcotest.(check bool) "no maintenance under a week" true (Summary.implied_maintenance_kcal intake short = None);
  Alcotest.(check bool) "no weights" true (Summary.weight_trend ~zone:Time.utc ~dates:(dates "2026-07-01" "2026-07-15") [] = None)

let () = Alcotest.run "summary" [
  ("time", [
    Alcotest.test_case "zone offsets" `Quick test_zone_offsets;
    Alcotest.test_case "local day bounds" `Quick test_local_day_bounds;
    Alcotest.test_case "date range" `Quick test_date_range ]);
  ("summary", [
    Alcotest.test_case "late meal lands on the local day" `Quick test_late_meal_lands_on_the_local_day;
    Alcotest.test_case "intake ignores unlogged days" `Quick test_intake_ignores_unlogged_days_and_counts_targets;
    Alcotest.test_case "weight trend and maintenance" `Quick test_weight_trend_and_maintenance ]) ]
