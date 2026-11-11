module Calories : sig
  type t = private int

  val of_int : int -> (t, Error.t) result
end

module Grams : sig
  type t = private float

  val of_float : float -> (t, Error.t) result
end

module Kilograms : sig
  type t = private float

  val of_float : float -> (t, Error.t) result
end

module Confidence : sig
  type t = private float

  val of_float : float -> (t, Error.t) result
end
