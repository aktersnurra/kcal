module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

module Calorie_target = struct
  type t = int

  let of_kcal kcal =
    if kcal > 0 then Ok kcal
    else
      Error
        (Error.Invalid_field
           { field = Numeric Calorie_target_kcal; reason = Nonpositive })
end

module Protein_target = struct
  type t = float

  let of_g g =
    if not (Float.is_finite g) then
      Error
        (Error.Invalid_field
           { field = Numeric Protein_target_g; reason = Nonfinite })
    else if Float.(g <= 0.) then
      Error
        (Error.Invalid_field
           { field = Numeric Protein_target_g; reason = Nonpositive })
    else Ok g
end

type daily_weight = { weigh_in_count : int; mean_weight_kg : float }

type day = {
  date : Date.t;
  meals : Meal.totals option;
  weight : daily_weight option;
}

let finite operation value =
  if Float.is_finite value then Ok value
  else Error (Error.Unrepresentable_result operation)

let add_calories left right =
  if left > Int.max_value - right then
    Error (Error.Numeric_overflow Calories_kcal)
  else Ok (left + right)

let add_optional left right =
  match (left, right) with
  | None, other | other, None -> Ok other
  | Some a, Some b ->
      Result.map (finite Error.Daily_meals (a +. b)) ~f:Option.some

let add_meal (meal : Meal.t) :
    Meal.totals option -> (Meal.totals, Error.t) Result.t = function
  | None ->
      Ok
        Meal.
          {
            meal_count = 1;
            calories_kcal = meal.calories_kcal;
            protein_g = meal.protein_g;
            carbs_g = meal.carbs_g;
            fat_g = meal.fat_g;
          }
  | Some totals ->
      let* calories_kcal =
        add_calories totals.calories_kcal meal.calories_kcal
      in
      let* protein_g =
        finite Error.Daily_meals (totals.protein_g +. meal.protein_g)
      in
      let* carbs_g = add_optional totals.carbs_g meal.carbs_g in
      let* fat_g = add_optional totals.fat_g meal.fat_g in
      Ok
        Meal.
          {
            meal_count = totals.meal_count + 1;
            calories_kcal;
            protein_g;
            carbs_g;
            fat_g;
          }

let add_weigh_in (weigh_in : Weigh_in.t) = function
  | None -> Ok { weigh_in_count = 1; mean_weight_kg = weigh_in.weight_kg }
  | Some { weigh_in_count; mean_weight_kg } ->
      let weigh_in_count = weigh_in_count + 1 in
      let* mean_weight_kg =
        finite Error.Daily_weight
          (mean_weight_kg
          +. (weigh_in.weight_kg -. mean_weight_kg)
             /. Float.of_int weigh_in_count)
      in
      Ok { weigh_in_count; mean_weight_kg }

module Date_key = struct
  module T = struct
    type t = Date.t

    let compare = Date.compare
    let sexp_of_t date = Sexp.Atom (Date.to_string date)
  end

  include T
  include Comparator.Make (T)
end

(* Out-of-range observations must not affect arithmetic or uncertainty. *)
let bucket zone range ~instant ~add records =
  let* start, stop = Date_range.utc_bounds zone range in
  List.fold_result records
    ~init:(Map.empty (module Date_key))
    ~f:(fun buckets record ->
      let time = instant record in
      if Ptime.compare start time <= 0 && Ptime.compare time stop < 0 then
        let* date = Time.local_date zone time in
        let* value = add record (Map.find buckets date) in
        Ok (Map.set buckets ~key:date ~data:value)
      else Ok buckets)

let daily_series zone range ~meals ~weigh_ins =
  let* meal_totals =
    bucket zone range
      ~instant:(fun (meal : Meal.t) -> meal.eaten_at)
      ~add:add_meal meals
  in
  let* weights =
    bucket zone range
      ~instant:(fun (weigh_in : Weigh_in.t) -> weigh_in.measured_at)
      ~add:add_weigh_in weigh_ins
  in
  Ok
    (List.map (Date_range.dates range) ~f:(fun date ->
         {
           date;
           meals = Map.find meal_totals date;
           weight = Map.find weights date;
         }))

type logged_intake = {
  logged_day_count : int;
  avg_calories_kcal : float;
  avg_protein_g : float;
}

type 'target target_result = { target : 'target; days_met : int }

(* Online means avoid overflowing a sum whose average is representable. All
   observations are nonnegative, so the difference is also representable. *)
let mean values ~f =
  List.foldi values ~init:0. ~f:(fun index average value ->
      average +. ((f value -. average) /. Float.of_int (index + 1)))

let logged_intake (logged : Meal.totals list) =
  match logged with
  | [] -> Ok None
  | _ ->
      let* avg_calories_kcal =
        finite Error.Average_intake
          (mean logged ~f:(fun (totals : Meal.totals) ->
               Float.of_int totals.calories_kcal))
      in
      let* avg_protein_g =
        finite Error.Average_intake
          (mean logged ~f:(fun (totals : Meal.totals) -> totals.protein_g))
      in
      Ok
        (Some
           {
             logged_day_count = List.length logged;
             avg_calories_kcal;
             avg_protein_g;
           })

let target_result logged ~is_met target =
  { target; days_met = List.count logged ~f:(is_met target) }

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

let days_since origin time =
  Ptime.Span.to_float_s (Ptime.diff time origin)
  /. Float.of_int Time.seconds_per_day

(* Least squares Sxy/Sxx, with weights scaled by their maximum before fitting.
   This keeps both the mean and covariance finite for extreme valid readings. *)
let least_squares_slope points =
  let scale =
    List.fold points ~init:0. ~f:(fun scale (_, y) -> Float.max scale y)
  in
  let points = List.map points ~f:(fun (x, y) -> (x, y /. scale)) in
  let mean_x = mean points ~f:fst and mean_y = mean points ~f:snd in
  let sxx =
    List.fold points ~init:0. ~f:(fun sum (x, _) ->
        sum +. Float.square (x -. mean_x))
  in
  let sxy =
    List.fold points ~init:0. ~f:(fun sum (x, y) ->
        sum +. ((x -. mean_x) *. (y -. mean_y)))
  in
  finite Error.Weight_regression (sxy /. sxx *. 7. *. scale)

let weight_trend zone range (weigh_ins : Weigh_in.t list) =
  let* start, stop = Date_range.utc_bounds zone range in
  let sorted =
    List.stable_sort
      ~compare:(fun (a : Weigh_in.t) (b : Weigh_in.t) ->
        Ptime.compare a.measured_at b.measured_at)
      (List.filter weigh_ins ~f:(fun weigh_in ->
           Ptime.compare start weigh_in.measured_at <= 0
           && Ptime.compare weigh_in.measured_at stop < 0))
  in
  match (sorted, List.rev sorted) with
  | [], _ | _, [] -> Ok None
  | first :: _, last :: _ ->
      let span_days = days_since first.measured_at last.measured_at in
      if Float.(span_days <= 0.) then
        Ok
          (Some
             (Single_reading
                { weight_kg = last.weight_kg; measured_at = last.measured_at }))
      else
        let points =
          List.map sorted ~f:(fun (w : Weigh_in.t) ->
              (days_since first.measured_at w.measured_at, w.weight_kg))
        in
        let* slope_kg_per_week = least_squares_slope points in
        Ok
          (Some
             (Trend
                {
                  weigh_in_count = List.length sorted;
                  first_kg = first.weight_kg;
                  last_kg = last.weight_kg;
                  change_kg = last.weight_kg -. first.weight_kg;
                  span_days;
                  slope_kg_per_week;
                }))

type maintenance_unavailable =
  | No_intake_logged
  | No_weight_trend
  | Span_too_short of { span_days : float; required_days : float }

type implied_maintenance =
  | Estimate of { kcal : float }
  | Unavailable of maintenance_unavailable

(* Wishnofsky (1958), 3500 kcal/lb in metric: a rough guide, not a
   physiological model (Hall et al., Int J Obes, 2008). *)
let kcal_per_kg = 7700.0
let required_span_days = 7.0

let implied_maintenance logged_intake weight_trend =
  match (logged_intake, weight_trend) with
  | None, _ -> Ok (Unavailable No_intake_logged)
  | Some _, (None | Some (Single_reading _)) -> Ok (Unavailable No_weight_trend)
  | Some _, Some (Trend { span_days; _ })
    when Float.(span_days < required_span_days) ->
      Ok
        (Unavailable
           (Span_too_short { span_days; required_days = required_span_days }))
  | Some { avg_calories_kcal; _ }, Some (Trend { slope_kg_per_week; _ }) ->
      let* kcal =
        finite Error.Implied_maintenance
          (avg_calories_kcal -. (slope_kg_per_week /. 7.0 *. kcal_per_kg))
      in
      Ok (Estimate { kcal })

type t = {
  day_count : int;
  logged_intake : logged_intake option;
  calorie_target : Calorie_target.t target_result option;
  protein_target : Protein_target.t target_result option;
  weight_trend : weight_trend option;
  implied_maintenance : implied_maintenance;
}

let summarise ?calorie_target ?protein_target zone range ~meals ~weigh_ins =
  let* series = daily_series zone range ~meals ~weigh_ins:[] in
  let logged = List.filter_map series ~f:(fun day -> day.meals) in
  let* logged_intake = logged_intake logged in
  let* weight_trend = weight_trend zone range weigh_ins in
  let* implied_maintenance = implied_maintenance logged_intake weight_trend in
  Ok
    {
      day_count = Date_range.day_count range;
      logged_intake;
      calorie_target =
        Option.map calorie_target
          ~f:
            (target_result logged ~is_met:(fun target (totals : Meal.totals) ->
                 totals.calories_kcal <= target));
      protein_target =
        Option.map protein_target
          ~f:
            (target_result logged ~is_met:(fun target (totals : Meal.totals) ->
                 Float.(totals.protein_g >= target)));
      weight_trend;
      implied_maintenance;
    }
