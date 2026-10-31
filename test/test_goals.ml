open Base

let ok result = Option.value_exn (Result.ok result)
let time value = ok (Time.parse_offset_datetime value)
let date value = ok (Date.of_string value)
let stockholm = ok (Time.parse_zone "Europe/Stockholm")
let window start end_ = ok (Goals.Eating_window.of_strings ~start ~end_)

let range first last =
  ok (Date_range.make ~first:(date first) ~last:(date last))

let goals ?eating_window ?target_weight_kg () : Goals.t =
  {
    timezone = stockholm;
    weekday_calories = ok (Goals.Calorie_target.of_kcal 1750);
    weekend_calories = ok (Goals.Calorie_target.of_kcal 1950);
    protein = Some (ok (Goals.Protein_target.of_g 140.));
    eating_window;
    target_weight_kg =
      Option.map target_weight_kg ~f:(fun kg ->
          ok (Nutrition.Kilograms.of_float kg));
  }

let meal at calories_kcal : Meal.t =
  let at = time at in
  let item =
    ok
      (Meal_item.of_storage ~id:(Meal_item_id.fresh ()) ~description:"m"
         ~calories_kcal ~protein_g:30. ~carbs_g:None ~fat_g:None
         ~confidence:None ~estimate_source:None ~notes:None ~product:None
         ~created_at:at ~updated_at:at)
  in
  ok
    (Meal.of_storage ~id:(Meal_id.fresh ()) ~user_id:(User_id.fresh ())
       ~eaten_at:at ~label:None ~items:[ item ] ~created_at:at ~updated_at:at
       ~deleted_at:None)

let weigh_in at weight_kg : Weigh_in.t =
  let at = time at in
  ok
    (Weigh_in.of_storage ~id:(Weigh_in_id.fresh ()) ~user_id:(User_id.fresh ())
       ~measured_at:at ~weight_kg ~source:Weigh_in.Manual ~created_at:at
       ~updated_at:at ~deleted_at:None)

let instant = Alcotest.testable Ptime.pp Ptime.equal

let test_eating_window_parsing () =
  let rejected start end_ =
    Result.is_error (Goals.Eating_window.of_strings ~start ~end_)
  in
  Alcotest.(check int)
    "16:8 is eight hours" (8 * 60)
    (Goals.Eating_window.duration_minutes (window "12:00" "20:00"));
  Alcotest.(check int)
    "crossing midnight" (6 * 60)
    (Goals.Eating_window.duration_minutes (window "22:00" "04:00"));
  List.iter
    [
      ("24:00", "20:00"); ("12:60", "20:00"); ("12", "20:00"); ("12:00", "12:00");
    ]
    ~f:(fun (start, end_) ->
      Alcotest.(check bool)
        (start ^ "-" ^ end_ ^ " rejected")
        true (rejected start end_))

(* 16:8 in Stockholm summer time: 12:00-20:00 local is 10:00-18:00 UTC. *)
let test_eating_window_status () =
  let status now =
    ok
      (Goals.Eating_window.status (window "12:00" "20:00") stockholm
         ~now:(time now))
  in
  (match status "2026-07-15T09:00:00Z" with
  | Closed { opens_at } ->
      Alcotest.check instant "opens at noon"
        (time "2026-07-15T10:00:00Z")
        opens_at
  | Open _ -> Alcotest.fail "closed before noon");
  (match status "2026-07-15T12:00:00Z" with
  | Open { closes_at } ->
      Alcotest.check instant "closes at eight"
        (time "2026-07-15T18:00:00Z")
        closes_at
  | Closed _ -> Alcotest.fail "open at two");
  (match status "2026-07-15T19:00:00Z" with
  | Closed { opens_at } ->
      Alcotest.check instant "opens tomorrow"
        (time "2026-07-16T10:00:00Z")
        opens_at
  | Open _ -> Alcotest.fail "closed at nine");
  let late = window "22:00" "04:00" in
  match
    ok
      (Goals.Eating_window.status late stockholm
         ~now:(time "2026-07-15T21:00:00Z"))
  with
  | Open { closes_at } ->
      Alcotest.check instant "a crossing window closes the next morning"
        (time "2026-07-16T02:00:00Z")
        closes_at
  | Closed _ -> Alcotest.fail "open at 23:00"

let test_weekend_target () =
  let goals = goals () in
  Alcotest.(check (list int))
    "Friday, Saturday, Sunday, Monday" [ 1750; 1950; 1950; 1750 ]
    (List.map [ "2026-10-30"; "2026-10-31"; "2026-11-01"; "2026-11-02" ]
       ~f:(fun day -> (Goals.calorie_target_on goals (date day) :> int)))

let test_summary_against_goals () =
  let meals =
    [
      (* Friday: inside the window, under target. *)
      meal "2026-10-30T11:00:00Z" 1600;
      (* Saturday: one meal at 22:30 local, outside 12-20, over target. *)
      meal "2026-10-31T13:00:00Z" 1500;
      meal "2026-10-31T21:30:00Z" 600;
    ]
  in
  let weigh_ins =
    [
      weigh_in "2026-10-24T06:00:00Z" 80.0; weigh_in "2026-10-31T06:00:00Z" 79.5;
    ]
  in
  let summary =
    ok
      (Summary.summarise
         ~goals:
           (goals ~eating_window:(window "12:00" "20:00") ~target_weight_kg:70.
              ())
         stockholm
         (range "2026-10-24" "2026-10-31")
         ~meals ~weigh_ins)
  in
  let calories = Option.value_exn summary.calorie_target in
  Alcotest.(check int) "one day under" 1 calories.days_at_or_under;
  Alcotest.(check (float 1e-9))
    "mean of -150 and +150" 0. calories.avg_vs_target_kcal;
  Alcotest.(check (option int))
    "one day inside the window" (Some 1)
    (Option.map summary.eating_window ~f:(fun w -> w.days_within));
  let target = Option.value_exn summary.target_weight in
  Alcotest.(check (float 1e-9)) "remaining" 9.5 target.remaining_kg;
  Alcotest.(check (option (float 1e-6)))
    "19 weeks at 0.5 kg/week" (Some 19.) target.weeks_to_target

let test_target_unreachable_on_current_trend () =
  let summary =
    ok
      (Summary.summarise
         ~goals:(goals ~target_weight_kg:70. ())
         stockholm
         (range "2026-10-24" "2026-10-31")
         ~meals:[]
         ~weigh_ins:
           [
             weigh_in "2026-10-24T06:00:00Z" 79.5;
             weigh_in "2026-10-31T06:00:00Z" 80.0;
           ])
  in
  Alcotest.(check (option (float 1e-9)))
    "gaining weight never reaches a lower target" None
    (Option.bind summary.target_weight ~f:(fun t -> t.weeks_to_target))

let test_context () =
  let now = time "2026-10-30T14:00:00Z" in
  let context =
    ok
      (Context.build ~now ~zone:stockholm
         ~goals:(Some (goals ~eating_window:(window "12:00" "20:00") ()))
         ~meals:[ meal "2026-10-30T11:30:00Z" 600 ]
         ~weigh_ins:[] ~latest_weight:None ~recent_products:[])
  in
  Alcotest.(check (option int))
    "remaining" (Some 1150) context.today.remaining_kcal;
  Alcotest.(check (option (float 1e-9)))
    "protein remaining" (Some 110.) context.today.remaining_protein_g;
  let fasting = Option.value_exn context.fasting in
  (match fasting.status with
  | Open { closes_at } ->
      Alcotest.check instant "closes at 20:00 CET"
        (time "2026-10-30T19:00:00Z")
        closes_at
  | Closed _ -> Alcotest.fail "window open at 15:00");
  Alcotest.(check (option instant))
    "last meal"
    (Some (time "2026-10-30T11:30:00Z"))
    fasting.last_meal_at;
  let month_range, _ = context.last_four_weeks in
  Alcotest.(check string)
    "the 28 days end yesterday, excluding today's partial intake" "2026-10-29"
    (Date.to_string (Date_range.last month_range));
  let week_range, _ = context.last_week in
  Alcotest.(check (pair string string))
    "last week ends yesterday"
    ("2026-10-23", "2026-10-29")
    ( Date.to_string (Date_range.first week_range),
      Date.to_string (Date_range.last week_range) )

let () =
  Alcotest.run "goals"
    [
      ( "goals",
        [
          Alcotest.test_case "eating window parsing" `Quick
            test_eating_window_parsing;
          Alcotest.test_case "eating window status" `Quick
            test_eating_window_status;
          Alcotest.test_case "weekend target" `Quick test_weekend_target;
          Alcotest.test_case "summary against goals" `Quick
            test_summary_against_goals;
          Alcotest.test_case "target unreachable on current trend" `Quick
            test_target_unreachable_on_current_trend;
          Alcotest.test_case "context" `Quick test_context;
        ] );
    ]
