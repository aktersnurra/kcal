(** The user's standing targets. Days are judged in [timezone], which is also
    the default zone for every calendar-day tool. *)

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

module Eating_window : sig
  type t = private { start_minute : int; end_minute : int }
  (** Local minutes after midnight. The window is [\[start, end)]; an end before
      the start crosses midnight, as in 20:00–04:00. *)

  val of_strings : start:string -> end_:string -> (t, Error.t) result
  (** Two distinct [HH:MM] wall-clock times. *)

  val of_minutes : start_minute:int -> end_minute:int -> (t, Error.t) result
  val to_string : int -> string
  val duration_minutes : t -> int
  val contains : t -> Time.zone -> Ptime.t -> bool

  type status =
    | Open of { closes_at : Ptime.t }
    | Closed of { opens_at : Ptime.t }

  val status : t -> Time.zone -> now:Ptime.t -> (status, Error.t) result
end

(** How [set_goals] treats one optional goal. *)
type 'a change = 'a Change.t = Keep | Clear | Set of 'a

type patch = {
  timezone : Time.zone option;  (** [None] keeps the current timezone. *)
  weekday_calories : Calorie_target.t option;
  weekend_calories : Calorie_target.t option;
  protein : Protein_target.t change;
  eating_window : Eating_window.t change;
  target_weight_kg : Nutrition.Kilograms.t change;
  notes : string change;
}

type t = {
  timezone : Time.zone;
  weekday_calories : Calorie_target.t;  (** Monday through Friday. *)
  weekend_calories : Calorie_target.t;  (** Saturday and Sunday. *)
  protein : Protein_target.t option;
  eating_window : Eating_window.t option;
  target_weight_kg : Nutrition.Kilograms.t option;
  notes : string option;
      (** The user's own context for these goals, for the agent to read: why,
          how strict, what matters most. *)
}

val max_notes_length : int
val calorie_target_on : t -> Date.t -> Calorie_target.t

val apply : t option -> patch -> (t, Error.t) result
(** The current goals (if any) with [patch] applied. Without current goals, the
    patch must give the timezone and both calorie targets. *)
