module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val sexp_of_t : t -> Base.Sexp.t
  val t_of_sexp : Base.Sexp.t -> t
  val fresh : unit -> t
  val of_string : string -> t option
  val to_string : t -> string
end

module Make () : S
