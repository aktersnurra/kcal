(** A non-empty, inclusive run of consecutive calendar dates. *)
type t

val max_day_count : int

val make : first:Date.t -> last:Date.t -> (t, Error.t) result
(** Fails when [last] is before [first] or the range exceeds [max_day_count] days. *)

val first : t -> Date.t
val last : t -> Date.t
val day_count : t -> int
val dates : t -> Date.t list
val mem : t -> Date.t -> bool

val utc_bounds : Time.zone -> t -> Ptime.t * Ptime.t
(** [start, stop) covering every local day in the range, as UTC instants. *)
