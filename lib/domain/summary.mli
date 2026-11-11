(** Pure aggregation over already-fetched meals and weigh-ins, bucketed into the
    local calendar days of a zone. Records outside the range are ignored. *)

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
(** One [day] per date in the range, in order. Fails if the zoned UTC bounds
    cannot be represented, even for an empty input. *)

type logged_intake = {
  logged_day_count : int;
  avg_calories_kcal : float;
  avg_protein_g : float;
}

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

type calorie_adherence = {
  days_at_or_under : int;
  avg_vs_target_kcal : float;
      (** Mean of each logged day's intake minus that day's target; negative is
          under. *)
}

type protein_adherence = { days_at_or_over : int }

type window_adherence = { days_within : int }
(** Logged days whose meals all fall inside the eating window. *)

type target_weight = {
  target_kg : float;
  remaining_kg : float;  (** Latest weight minus target. *)
  weeks_to_target : float option;
      (** At the current slope; [None] unless the trend heads toward it. *)
}

type t = {
  day_count : int;
  logged_intake : logged_intake option;
      (** [None]: no day in the range has a meal. *)
  calorie_target : calorie_adherence option;
      (** Against each day's weekday or weekend target. *)
  protein_target : protein_adherence option;
  eating_window : window_adherence option;
  weight_trend : weight_trend option;  (** [None]: no weigh-ins in the range. *)
  target_weight : target_weight option;
  implied_maintenance : implied_maintenance;
}

val summarise :
  ?goals:Goals.t ->
  Time.zone ->
  Date_range.t ->
  meals:Meal.t list ->
  weigh_ins:Weigh_in.t list ->
  (t, Error.t) result
(** Target fields are [None] without [goals] or without that goal. *)
