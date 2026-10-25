type t
(** A calendar date, with no time of day and no zone. *)

val of_string : string -> (t, Error.t) result
(** Parses [YYYY-MM-DD], including years 0000 through 9999. *)

val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int

val of_instant : utc_offset_s:int -> Ptime.t -> (t, Error.t) result
(** The local date at [utc_offset_s] seconds east of UTC; fails rather than
    accepting Ptime's UTC fallback at calendar endpoints. *)

val add_days : t -> int -> (t, Error.t) result
(** Fails if the resulting date leaves years 0000 through 9999. *)

val days_between : first:t -> last:t -> int
(** [last] minus [first], in days; negative when [last] is earlier. *)

val midnight_utc : t -> Ptime.t
