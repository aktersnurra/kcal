(** Pure aggregation over already-fetched meals and weigh-ins, bucketed into the
    local calendar days of a zone. Records outside the range are ignored. *)

module Calorie_target : sig
  type t = private int

  val of_kcal : int -> (t, Error.t) result
  (** Positive calories. *)
end

module Protein_target : sig
  type t = private float

  val of_g : float -> (t, Error.t) result
  (** Finite, positive grams. *)
end

type daily_weight = { weigh_in_count : int; mean_weight_kg : float }

type day = {
  date : Date.t;
  meals : Meal.totals option;
      (** [None]: nothing logged, so intake is unknown, not zero. *)
  weight : daily_weight option;
}

val daily_series :
  Time.zone ->
  Date_range.t ->
  meals:Meal.t list ->
  weigh_ins:Weigh_in.t list ->
  (day list, Error.t) result
(** One [day] per date in the range, in order. *)

type logged_intake = {
  logged_day_count : int;
  avg_calories_kcal : float;
  avg_protein_g : float;
}

type 'target target_result = { target : 'target; days_met : int }

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

type maintenance_unavailable =
  | No_intake_logged
  | No_weight_trend
  | Span_too_short of { span_days : float; required_days : float }

type implied_maintenance =
  | Estimate of { kcal : float }
  | Unavailable of maintenance_unavailable

type t = {
  day_count : int;
  logged_intake : logged_intake option;
      (** [None]: no day in the range has a meal. *)
  calorie_target : Calorie_target.t target_result option;
      (** Days at or under the target. *)
  protein_target : Protein_target.t target_result option;
      (** Days at or over the target. *)
  weight_trend : weight_trend option;  (** [None]: no weigh-ins in the range. *)
  implied_maintenance : implied_maintenance;
}

val summarise :
  ?calorie_target:Calorie_target.t ->
  ?protein_target:Protein_target.t ->
  Time.zone ->
  Date_range.t ->
  meals:Meal.t list ->
  weigh_ins:Weigh_in.t list ->
  (t, Error.t) result
