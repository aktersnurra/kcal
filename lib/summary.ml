module Calorie_target = struct
  type t = int
  let of_kcal kcal = if kcal > 0 then Some kcal else None
end

module Protein_target = struct
  type t = float
  let of_g g = if g > 0.0 then Some g else None
end

type daily_weight = { weigh_in_count : int; mean_weight_kg : float }
type day = { date : Date.t; meals : Meal.totals option; weight : daily_weight option }

let add_optional left right =
  match left, right with
  | None, other | other, None -> other
  | Some a, Some b -> Some (a +. b)

let add_meal (meal : Meal.t) : Meal.totals option -> Meal.totals = function
  | None ->
      { meal_count = 1; calories_kcal = meal.calories_kcal; protein_g = meal.protein_g;
        carbs_g = meal.carbs_g; fat_g = meal.fat_g }
  | Some totals ->
      { meal_count = totals.meal_count + 1;
        calories_kcal = totals.calories_kcal + meal.calories_kcal;
        protein_g = totals.protein_g +. meal.protein_g;
        carbs_g = add_optional totals.carbs_g meal.carbs_g;
        fat_g = add_optional totals.fat_g meal.fat_g }

let add_weigh_in (weigh_in : Weigh_in.t) = function
  | None -> { weigh_in_count = 1; mean_weight_kg = weigh_in.weight_kg }
  | Some { weigh_in_count; mean_weight_kg } ->
      let weigh_in_count = weigh_in_count + 1 in
      { weigh_in_count;
        mean_weight_kg = mean_weight_kg +. ((weigh_in.weight_kg -. mean_weight_kg) /. float_of_int weigh_in_count) }

module Date_map = Map.Make (Date)

(* Folds each record into the entry for its local date, if that date is in range. *)
let bucket zone range ~instant ~add records =
  List.fold_left
    (fun buckets record ->
      let date = Time.local_date zone (instant record) in
      if Date_range.mem range date then Date_map.update date (fun entry -> Some (add record entry)) buckets else buckets)
    Date_map.empty records

let daily_series zone range ~meals ~weigh_ins =
  let meal_totals = bucket zone range ~instant:(fun (meal : Meal.t) -> meal.eaten_at) ~add:add_meal meals in
  let weights = bucket zone range ~instant:(fun (weigh_in : Weigh_in.t) -> weigh_in.measured_at) ~add:add_weigh_in weigh_ins in
  List.map
    (fun date -> { date; meals = Date_map.find_opt date meal_totals; weight = Date_map.find_opt date weights })
    (Date_range.dates range)

type logged_intake = { logged_day_count : int; avg_calories_kcal : float; avg_protein_g : float }
type 'target target_result = { target : 'target; days_met : int }

let logged_intake (logged : Meal.totals list) =
  match logged with
  | [] -> None
  | _ ->
      let logged_day_count = List.length logged in
      let average value = List.fold_left (fun sum totals -> sum +. value totals) 0.0 logged /. float_of_int logged_day_count in
      Some { logged_day_count;
             avg_calories_kcal = average (fun (totals : Meal.totals) -> float_of_int totals.calories_kcal);
             avg_protein_g = average (fun (totals : Meal.totals) -> totals.protein_g) }

let target_result logged ~is_met target =
  { target; days_met = List.length (List.filter (is_met target) logged) }

type weight_trend =
  | Single_reading of { weight_kg : float; measured_at : Ptime.t }
  | Trend of {
      weigh_in_count : int;
      first_kg : float;
      last_kg : float;
      change_kg : float;
      span_days : float;
      slope_kg_per_week : float;
    }

let days_since origin time = Ptime.Span.to_float_s (Ptime.diff time origin) /. float_of_int Time.seconds_per_day

(* Simple linear regression of weight on time: the least-squares slope is
   Sxy / Sxx over the points' deviations from their means. *)
let least_squares_slope points =
  let count = float_of_int (List.length points) in
  let mean value = List.fold_left (fun sum point -> sum +. value point) 0.0 points /. count in
  let mean_x = mean fst and mean_y = mean snd in
  let sxx = List.fold_left (fun sum (x, _) -> sum +. ((x -. mean_x) ** 2.)) 0.0 points in
  let sxy = List.fold_left (fun sum (x, y) -> sum +. ((x -. mean_x) *. (y -. mean_y))) 0.0 points in
  sxy /. sxx

let weight_trend zone range (weigh_ins : Weigh_in.t list) =
  let in_range (weigh_in : Weigh_in.t) = Date_range.mem range (Time.local_date zone weigh_in.measured_at) in
  let sorted =
    List.stable_sort (fun (a : Weigh_in.t) (b : Weigh_in.t) -> Ptime.compare a.measured_at b.measured_at)
      (List.filter in_range weigh_ins)
  in
  match sorted, List.rev sorted with
  | [], _ | _, [] -> None
  | first :: _, last :: _ ->
      let span_days = days_since first.measured_at last.measured_at in
      (* With no time between the readings there is nothing to fit a slope to. *)
      if span_days <= 0.0 then Some (Single_reading { weight_kg = last.weight_kg; measured_at = last.measured_at })
      else
        let points = List.map (fun (w : Weigh_in.t) -> (days_since first.measured_at w.measured_at, w.weight_kg)) sorted in
        Some
          (Trend
             { weigh_in_count = List.length sorted;
               first_kg = first.weight_kg;
               last_kg = last.weight_kg;
               change_kg = last.weight_kg -. first.weight_kg;
               span_days;
               slope_kg_per_week = least_squares_slope points *. 7.0 })

type maintenance_unavailable =
  | No_intake_logged
  | No_weight_trend
  | Span_too_short of { span_days : float; required_days : float }

type implied_maintenance = Estimate of { kcal : float } | Unavailable of maintenance_unavailable

(* The 3500 kcal/lb rule of thumb (Wishnofsky, 1958) in metric.  Hall et al.
   (Int J Obes, 2008) show it overstates long-run change, so the estimate is
   a rough guide, best over a few weeks. *)
let kcal_per_kg = 7700.0
let required_span_days = 7.0

(* Intake that would hold weight steady: average intake minus the energy
   stored in (or released by) the weight trend. *)
let implied_maintenance logged_intake weight_trend =
  match logged_intake, weight_trend with
  | None, _ -> Unavailable No_intake_logged
  | Some _, (None | Some (Single_reading _)) -> Unavailable No_weight_trend
  | Some _, Some (Trend { span_days; _ }) when span_days < required_span_days ->
      Unavailable (Span_too_short { span_days; required_days = required_span_days })
  | Some { avg_calories_kcal; _ }, Some (Trend { slope_kg_per_week; _ }) ->
      Estimate { kcal = avg_calories_kcal -. (slope_kg_per_week /. 7.0 *. kcal_per_kg) }

type t = {
  day_count : int;
  logged_intake : logged_intake option;
  calorie_target : Calorie_target.t target_result option;
  protein_target : Protein_target.t target_result option;
  weight_trend : weight_trend option;
  implied_maintenance : implied_maintenance;
}

let summarise ?calorie_target ?protein_target zone range ~meals ~weigh_ins =
  let logged = List.filter_map (fun day -> day.meals) (daily_series zone range ~meals ~weigh_ins:[]) in
  let logged_intake = logged_intake logged in
  let weight_trend = weight_trend zone range weigh_ins in
  { day_count = Date_range.day_count range;
    logged_intake;
    calorie_target =
      Option.map (target_result logged ~is_met:(fun target (totals : Meal.totals) -> totals.calories_kcal <= target)) calorie_target;
    protein_target =
      Option.map (target_result logged ~is_met:(fun target (totals : Meal.totals) -> totals.protein_g >= target)) protein_target;
    weight_trend;
    implied_maintenance = implied_maintenance logged_intake weight_trend }
