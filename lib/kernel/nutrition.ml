module Domain_error = Error
open Base
module Error = Domain_error

let invalid field reason =
  Error (Error.Invalid_field { field = Numeric field; reason })

module Calories = struct
  type t = int

  let of_int value =
    if value < 0 then invalid Calories_kcal Negative else Ok value
end

module Grams = struct
  type t = float

  let of_float value =
    if not (Float.is_finite value) then invalid Protein_g Nonfinite
    else if Float.(value < 0.) then invalid Protein_g Negative
    else Ok value
end

module Kilograms = struct
  type t = float

  let of_float value =
    if not (Float.is_finite value) then invalid Weight_kg Nonfinite
    else if Float.(value <= 0.) then invalid Weight_kg Nonpositive
    else Ok value
end

module Confidence = struct
  type t = float

  let of_float value =
    if not (Float.is_finite value) then invalid Confidence Nonfinite
    else if Float.(value < 0. || value > 1.) then
      invalid Confidence Out_of_range
    else Ok value
end
