module Key : sig
  type t

  val of_bytes : bytes -> (t, Error.t) result
end

val encrypt : key:Key.t -> string -> (string, Error.t) result
val decrypt : key:Key.t -> string -> (string, Error.t) result
