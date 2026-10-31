val parse_offset_datetime : string -> (Ptime.t, Error.t) result
val to_utc_string : Ptime.t -> string
val seconds_per_day : int

type zone
(** Where calendar days begin and end. *)

val utc : zone

val parse_zone : string -> (zone, Error.t) result
(** A name from [supported_zone_names], or a fixed offset such as [+02:00] or
    [Z]. *)

val supported_zone_names : string list
val zone_name : zone -> string
val utc_offset_s_at : zone -> Ptime.t -> int

val local_date : zone -> Ptime.t -> (Date.t, Error.t) result
(** Fails if the zone places the instant outside the supported local calendar.
*)

val local_day_bounds : zone -> Date.t -> (Ptime.t * Ptime.t, Error.t) result
(** The local day's UTC [(start, stop)] bounds, with an exclusive [stop]. Fails
    if either bound or the following calendar date cannot be represented. *)

val local_minute_of_day : zone -> Ptime.t -> int
(** Minutes after local midnight, [0] through [1439]. *)

val local_instant :
  zone -> Date.t -> minute_of_day:int -> (Ptime.t, Error.t) result
(** The instant a local wall-clock time occurs on [date]. *)
