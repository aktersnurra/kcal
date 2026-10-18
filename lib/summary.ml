(* Pure aggregation over already-fetched meals and weigh-ins.  Instants are
   bucketed into calendar days of the caller's [zone]. *)

type day = {
  date : string;
  meal_count : int;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
}

let add_optional left right =
  match left, right with
  | None, other | other, None -> other
  | Some a, Some b -> Some (a +. b)

let empty_day date =
  { date; meal_count = 0; calories_kcal = 0; protein_g = 0.0; carbs_g = None; fat_g = None }

(* One entry per date in [dates], in order, including days with no meals. *)
let daily_series ~zone ~dates (meals : Meal.t list) =
  let days = Hashtbl.create 64 in
  List.iter (fun date -> Hashtbl.replace days date (empty_day date)) dates;
  List.iter
    (fun (meal : Meal.t) ->
      let date = Time.local_date_string zone meal.eaten_at in
      match Hashtbl.find_opt days date with
      | None -> ()
      | Some day ->
          Hashtbl.replace days date
            { day with
              meal_count = day.meal_count + 1;
              calories_kcal = day.calories_kcal + meal.calories_kcal;
              protein_g = day.protein_g +. meal.protein_g;
              carbs_g = add_optional day.carbs_g meal.carbs_g;
              fat_g = add_optional day.fat_g meal.fat_g })
    meals;
  List.map (fun date -> Hashtbl.find days date) dates

type intake = {
  days : int;
  logged_days : int;
  avg_calories_kcal : float option;
  avg_protein_g : float option;
  days_at_or_under_calorie_target : int option;
  days_at_or_over_protein_target : int option;
}

(* Averages and target hit-counts cover only days with at least one meal;
   a day with nothing logged is unknown, not zero intake. *)
let intake ?calorie_target ?protein_target series =
  let logged = List.filter (fun day -> day.meal_count > 0) series in
  let logged_days = List.length logged in
  let average f =
    if logged_days = 0 then None
    else Some (List.fold_left (fun sum day -> sum +. f day) 0.0 logged /. float_of_int logged_days)
  in
  let count predicate = List.length (List.filter predicate logged) in
  { days = List.length series;
    logged_days;
    avg_calories_kcal = average (fun day -> float_of_int day.calories_kcal);
    avg_protein_g = average (fun day -> day.protein_g);
    days_at_or_under_calorie_target =
      Option.map (fun target -> count (fun day -> day.calories_kcal <= target)) calorie_target;
    days_at_or_over_protein_target =
      Option.map (fun target -> count (fun day -> day.protein_g >= target)) protein_target }

type weight_trend = {
  weigh_in_count : int;
  first_kg : float;
  last_kg : float;
  change_kg : float;
  span_days : float;
  slope_kg_per_week : float option;
}

let seconds_per_day = 86400.0

(* Ordinary least squares of weight against time over weigh-ins whose local
   date is in [dates].  [None] when there are none. *)
let weight_trend ~zone ~dates (weights : Weigh_in.t list) =
  let in_range (weight : Weigh_in.t) = List.mem (Time.local_date_string zone weight.measured_at) dates in
  let sorted =
    List.sort (fun (a : Weigh_in.t) (b : Weigh_in.t) -> Ptime.compare a.measured_at b.measured_at)
      (List.filter in_range weights)
  in
  match sorted with
  | [] -> None
  | first :: _ ->
      let last = List.nth sorted (List.length sorted - 1) in
      let origin = Ptime.to_float_s first.measured_at in
      let points =
        List.map (fun (w : Weigh_in.t) -> ((Ptime.to_float_s w.measured_at -. origin) /. seconds_per_day, w.weight_kg)) sorted
      in
      let n = float_of_int (List.length points) in
      let mean f = List.fold_left (fun sum p -> sum +. f p) 0.0 points /. n in
      let mean_x = mean fst and mean_y = mean snd in
      let sxx = List.fold_left (fun sum (x, _) -> sum +. ((x -. mean_x) ** 2.)) 0.0 points in
      let sxy = List.fold_left (fun sum (x, y) -> sum +. ((x -. mean_x) *. (y -. mean_y))) 0.0 points in
      let span_days = (Ptime.to_float_s last.measured_at -. origin) /. seconds_per_day in
      Some
        { weigh_in_count = List.length sorted;
          first_kg = first.weight_kg;
          last_kg = last.weight_kg;
          change_kg = last.weight_kg -. first.weight_kg;
          span_days;
          slope_kg_per_week = (if sxx > 0.0 then Some (sxy /. sxx *. 7.0) else None) }

let kcal_per_kg = 7700.0

(* Intake that would hold weight steady: average intake minus the energy
   stored in (or released by) the weight trend.  Needs a week of weigh-ins to
   be meaningful, and is only as accurate as the logging is complete. *)
let implied_maintenance_kcal intake trend =
  match intake.avg_calories_kcal, trend with
  | Some average, Some { slope_kg_per_week = Some slope; span_days; _ } when span_days >= 7.0 ->
      Some (average -. (slope /. 7.0 *. kcal_per_kg))
  | _ -> None
