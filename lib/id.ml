open Base

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

module Make () : S = struct
  type t = string [@@deriving equal, compare, sexp_of]

  let generator = Uuidm.v4_gen (Stdlib.Random.State.make_self_init ())
  let fresh () = Uuidm.to_string (generator ())

  let of_string value =
    match Uuidm.of_string value with
    | None -> None
    | Some uuid -> Some (Uuidm.to_string uuid)

  let t_of_sexp sexp =
    match sexp with
    | Base.Sexp.Atom value -> (
        match of_string value with
        | Some id -> id
        | None -> Sexplib0.Sexp_conv.of_sexp_error "Invalid UUID" sexp)
    | Base.Sexp.List _ ->
        Sexplib0.Sexp_conv.of_sexp_error "Expected UUID atom" sexp

  let to_string value = value
end
