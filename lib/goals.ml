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

module Eating_window = struct
  type t = { start_minute : int; end_minute : int }

  let minutes_per_day = 24 * 60

  let of_minutes ~start_minute ~end_minute =
    let valid minute = minute >= 0 && minute < minutes_per_day in
    if not (valid start_minute && valid end_minute) then
      Error (Error.Rejected "eating window times must be within one day")
    else if start_minute = end_minute then
      Error (Error.Rejected "eating window start and end must differ")
    else Ok { start_minute; end_minute }

  let parse_minute value =
    let digits text =
      String.length text = 2 && String.for_all text ~f:Char.is_digit
    in
    match String.split value ~on:':' with
    | [ hours; minutes ] when digits hours && digits minutes ->
        let hours = Int.of_string hours and minutes = Int.of_string minutes in
        if hours < 24 && minutes < 60 then Ok ((hours * 60) + minutes)
        else Error (Error.Rejected (value ^ " is not a valid HH:MM time"))
    | _ -> Error (Error.Rejected (value ^ " is not a valid HH:MM time"))

  let of_strings ~start ~end_ =
    let* start_minute = parse_minute start in
    let* end_minute = parse_minute end_ in
    of_minutes ~start_minute ~end_minute

  let to_string minute = Printf.sprintf "%02d:%02d" (minute / 60) (minute % 60)

  let duration_minutes { start_minute; end_minute } =
    (end_minute - start_minute) % minutes_per_day

  let crosses_midnight window = window.end_minute < window.start_minute

  let contains_minute window minute =
    if crosses_midnight window then
      minute >= window.start_minute || minute < window.end_minute
    else minute >= window.start_minute && minute < window.end_minute

  let contains window zone time =
    contains_minute window (Time.local_minute_of_day zone time)

  type status =
    | Open of { closes_at : Ptime.t }
    | Closed of { opens_at : Ptime.t }

  let status window zone ~now =
    let* today = Time.local_date zone now in
    let at days minute_of_day =
      let* date = Date.add_days today days in
      Time.local_instant zone date ~minute_of_day
    in
    let minute = Time.local_minute_of_day zone now in
    if contains_minute window minute then
      (* Before midnight in a crossing window, it closes tomorrow. *)
      let days =
        if crosses_midnight window && minute >= window.start_minute then 1
        else 0
      in
      let* closes_at = at days window.end_minute in
      Ok (Open { closes_at })
    else
      let days = if minute < window.start_minute then 0 else 1 in
      let* opens_at = at days window.start_minute in
      Ok (Closed { opens_at })
end

type t = {
  timezone : Time.zone;
  weekday_calories : Calorie_target.t;
  weekend_calories : Calorie_target.t;
  protein : Protein_target.t option;
  eating_window : Eating_window.t option;
  target_weight_kg : Nutrition.Kilograms.t option;
  notes : string option;
}

(* Long enough for a few sentences of context, short enough that get_context
   stays small. *)
let max_notes_length = 2000

let calorie_target_on goals date =
  if Date.is_weekend date then goals.weekend_calories
  else goals.weekday_calories
