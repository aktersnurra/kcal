(** Everything an agent needs to orient itself in one call, assembled from
    already-fetched records. *)

type today = {
  date : Date.t;
  totals : Meal.totals option;  (** [None]: nothing logged yet. *)
  calorie_target : Goals.Calorie_target.t option;
  remaining_kcal : int option;
      (** Target minus intake so far; negative is over. *)
  remaining_protein_g : float option;
}

type fasting = {
  window : Goals.Eating_window.t;
  status : Goals.Eating_window.status;
  last_meal_at : Ptime.t option;  (** Within [history_days]. *)
}

type t = {
  now : Ptime.t;
  zone : Time.zone;
  goals : Goals.t option;
  today : today;
  fasting : fasting option;
  latest_weight : Weigh_in.t option;
  last_week : Date_range.t * Summary.t;
      (** The seven complete days before today: intake against targets. *)
  last_four_weeks : Date_range.t * Summary.t;
      (** The 28 days through today: weight trend and maintenance. *)
  recent_products : (Product.t * Ptime.t) list;
      (** With when each was last eaten. *)
}

val history_days : int
(** How far back [meals] and [weigh_ins] must reach, today included. *)

val history : Time.zone -> now:Ptime.t -> (Date_range.t, Error.t) result

val build :
  now:Ptime.t ->
  zone:Time.zone ->
  goals:Goals.t option ->
  meals:Meal.t list ->
  weigh_ins:Weigh_in.t list ->
  latest_weight:Weigh_in.t option ->
  recent_products:(Product.t * Ptime.t) list ->
  (t, Error.t) result
