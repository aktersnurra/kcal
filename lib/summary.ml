module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

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

type calorie_adherence = { days_at_or_under : int; avg_vs_target_kcal : float }
type protein_adherence = { days_at_or_over : int }
type window_adherence = { days_within : int }

type target_weight = {
  target_kg : float;
  remaining_kg : float;
  weeks_to_target : float option;
}

type t = {
  day_count : int;
  logged_intake : logged_intake option;
  calorie_target : calorie_adherence option;
  protein_target : protein_adherence option;
  eating_window : window_adherence option;
  weight_trend : weight_trend option;
  target_weight : target_weight option;
  implied_maintenance : implied_maintenance;
}

let calorie_adherence (goals : Goals.t) logged =
  match logged with
  | [] -> Ok None
  | _ ->
      let gap (date, (totals : Meal.totals)) =
        Float.of_int
          (totals.calories_kcal - (Goals.calorie_target_on goals date :> int))
      in
      let* avg_vs_target_kcal =
        finite Error.Average_intake (mean logged ~f:gap)
      in
      Ok
        (Some
           {
             days_at_or_under =
               List.count logged ~f:(fun (date, (totals : Meal.totals)) ->
                   totals.calories_kcal
                   <= (Goals.calorie_target_on goals date :> int));
             avg_vs_target_kcal;
           })

let latest_weight_kg = function
  | Single_reading { weight_kg; _ } -> weight_kg
  | Trend { last_kg; _ } -> last_kg

(* Weeks at the current slope, only while the trend heads toward the target. *)
let target_weight target_kg trend =
  let target_kg = (target_kg : Nutrition.Kilograms.t :> float) in
  let remaining_kg = latest_weight_kg trend -. target_kg in
  let weeks_to_target =
    match trend with
    | _ when Float.(remaining_kg = 0.) -> Some 0.
    | Trend { slope_kg_per_week; _ }
      when Float.(remaining_kg *. slope_kg_per_week < 0.) ->
        Some (remaining_kg /. Float.neg slope_kg_per_week)
    | _ -> None
  in
  { target_kg; remaining_kg; weeks_to_target }

let summarise ?goals zone range ~meals ~weigh_ins =
  let* series = daily_series zone range ~meals ~weigh_ins:[] in
  let logged =
    List.filter_map series ~f:(fun day ->
        Option.map day.meals ~f:(fun totals -> (day.date, totals)))
  in
  let* logged_intake = logged_intake (List.map logged ~f:snd) in
  let* weight_trend = weight_trend zone range weigh_ins in
  let* implied_maintenance = implied_maintenance logged_intake weight_trend in
  let* calorie_target =
    match goals with
    | None -> Ok None
    | Some goals -> calorie_adherence goals logged
  in
  let* eating_window =
    match
      Option.bind goals ~f:(fun (goals : Goals.t) -> goals.eating_window)
    with
    | None -> Ok None
    | Some window ->
        let* within =
          bucket zone range
            ~instant:(fun (meal : Meal.t) -> meal.eaten_at)
            ~add:(fun (meal : Meal.t) so_far ->
              Ok
                (Option.value so_far ~default:true
                && Goals.Eating_window.contains window zone meal.eaten_at))
            meals
        in
        Ok (Some { days_within = Map.count within ~f:Fn.id })
  in
  let protein_target =
    Option.bind goals ~f:(fun (goals : Goals.t) ->
        Option.map goals.protein ~f:(fun target ->
            {
              days_at_or_over =
                List.count logged ~f:(fun (_, (totals : Meal.totals)) ->
                    Float.(totals.protein_g >= (target :> float)));
            }))
  in
  let target_weight =
    match
      ( Option.bind goals ~f:(fun (goals : Goals.t) -> goals.target_weight_kg),
        weight_trend )
    with
    | Some target, Some trend -> Some (target_weight target trend)
    | _ -> None
  in
  Ok
    {
      day_count = Date_range.day_count range;
      logged_intake;
      calorie_target;
      protein_target;
      eating_window;
      weight_trend;
      target_weight;
      implied_maintenance;
    }
