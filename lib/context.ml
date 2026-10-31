module Domain_error = Error
open Base
module Error = Domain_error

let ( let* ) result f = Result.bind result ~f

type today = {
  date : Date.t;
  totals : Meal.totals option;
  calorie_target : Goals.Calorie_target.t option;
  remaining_kcal : int option;
  remaining_protein_g : float option;
}

type fasting = {
  window : Goals.Eating_window.t;
  status : Goals.Eating_window.status;
  last_meal_at : Ptime.t option;
}

type t = {
  now : Ptime.t;
  zone : Time.zone;
  goals : Goals.t option;
  today : today;
  fasting : fasting option;
  latest_weight : Weigh_in.t option;
  last_week : Date_range.t * Summary.t;
  last_four_weeks : Date_range.t * Summary.t;
  recent_products : (Product.t * Ptime.t) list;
}

let history_days = 28

(* [days] dates ending [offset] days from today. *)
let days_ending zone ~now ~offset days =
  let* today = Time.local_date zone now in
  let* last = Date.add_days today offset in
  let* first = Date.add_days last (1 - days) in
  Date_range.make ~first ~last

let history zone ~now = days_ending zone ~now ~offset:0 history_days

let today_of (goals : Goals.t option) (day : Summary.day) =
  let calorie_target =
    Option.map goals ~f:(fun goals -> Goals.calorie_target_on goals day.date)
  in
  let eaten f = Option.value_map day.meals ~default:0. ~f in
  {
    date = day.date;
    totals = day.meals;
    calorie_target;
    remaining_kcal =
      Option.map calorie_target ~f:(fun target ->
          (target :> int)
          - Option.value_map day.meals ~default:0
              ~f:(fun (totals : Meal.totals) -> totals.calories_kcal));
    remaining_protein_g =
      Option.bind goals ~f:(fun (goals : Goals.t) ->
          Option.map goals.protein ~f:(fun target ->
              (target :> float)
              -. eaten (fun (totals : Meal.totals) -> totals.protein_g)));
  }

let build ~now ~zone ~goals ~meals ~weigh_ins ~latest_weight ~recent_products =
  let* today_range = days_ending zone ~now ~offset:0 1 in
  let* week = days_ending zone ~now ~offset:(-1) 7 in
  let* four_weeks = history zone ~now in
  let* series = Summary.daily_series zone today_range ~meals ~weigh_ins:[] in
  let* today =
    match series with
    | [ day ] -> Ok (today_of goals day)
    | _ -> Error (Error.Unrepresentable_result Daily_meals)
  in
  let summarise range =
    Result.map (Summary.summarise ?goals zone range ~meals ~weigh_ins)
      ~f:(fun summary -> (range, summary))
  in
  let* last_week = summarise week in
  let* last_four_weeks = summarise four_weeks in
  let* fasting =
    match
      Option.bind goals ~f:(fun (goals : Goals.t) -> goals.eating_window)
    with
    | None -> Ok None
    | Some window ->
        let* status = Goals.Eating_window.status window zone ~now in
        let last_meal_at =
          List.filter_map meals ~f:(fun (meal : Meal.t) ->
              Option.some_if
                (Ptime.compare meal.eaten_at now <= 0)
                meal.eaten_at)
          |> List.max_elt ~compare:Ptime.compare
        in
        Ok (Some { window; status; last_meal_at })
  in
  Ok
    {
      now;
      zone;
      goals;
      today;
      fasting;
      latest_weight;
      last_week;
      last_four_weeks;
      recent_products;
    }
