(** How a partial update treats one optional field: an omitted field is kept,
    null clears it, and a value sets it. *)
type 'a t = Keep | Clear | Set of 'a

val apply : 'a option -> 'a t -> 'a option
(** The field's value after the change, given its current value. *)
