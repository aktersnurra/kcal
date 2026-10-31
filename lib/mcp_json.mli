val meal_json : Meal.t -> (Yojson.Safe.t, Error.t) result
(** Fails if the items' totals overflow. *)

val product_json : Product.t -> Yojson.Safe.t
val day_json : Summary.day -> Yojson.Safe.t
val range_json : Time.zone -> Date_range.t -> (string * Yojson.Safe.t) list
val summary_json : Time.zone -> Date_range.t -> Service.summary -> Yojson.Safe.t

val meal_with_revisions_json :
  Meal.t ->
  (Meal_item_id.t * Meal_item.revision) list ->
  (Yojson.Safe.t, Error.t) result

val goals_json : Goals.t -> Yojson.Safe.t
val context_json : Context.t -> Yojson.Safe.t
val correction_json : Service.product_correction -> Yojson.Safe.t
val weight_json : Weigh_in.t -> Yojson.Safe.t
val withings_status_json : Withings_connection.status -> Yojson.Safe.t
val text_result : Yojson.Safe.t -> Yojson.Safe.t
val tool_error : string -> Yojson.Safe.t
val cacheable : (string * Yojson.Safe.t) list -> Yojson.Safe.t
