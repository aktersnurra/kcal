type t
(** Which keys have a run in flight, so repeated requests for the same key do
    not pile up concurrent runs. Not safe across domains. *)

val create : unit -> t

val request :
  t -> key:string -> schedule:((unit -> unit) -> unit) -> (unit -> unit) -> unit
(** Hands [run] to [schedule] unless a run for [key] is already scheduled or
    under way. In that case nothing more is scheduled; the run in flight is
    followed by exactly one more, however many requests arrived meanwhile, so
    none of them is lost. *)
