module type S = sig
  type t

  val fresh : unit -> t
  val of_string : string -> t option
  val to_string : t -> string
end

module Make () : S = struct
  type t = string

  let generator = Uuidm.v4_gen (Random.State.make_self_init ())

  let fresh () = Uuidm.to_string (generator ())

  let of_string value =
    match Uuidm.of_string value with
    | None -> None
    | Some uuid -> Some (Uuidm.to_string uuid)

  let to_string value = value
end
