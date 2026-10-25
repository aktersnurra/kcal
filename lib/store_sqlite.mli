type t

val resolve_user :
  t -> issuer:string -> subject:string -> (User.t, Error.t) result

val save_product :
  t ->
  user:User.t ->
  id:Product_id.t option ->
  Product.Create.t ->
  (Product.t, Error.t) result

val search_products :
  t ->
  user:User.t ->
  query:Search_query.t ->
  limit:int ->
  (Product.t list, Error.t) result

val create_meal : t -> user:User.t -> Meal.Create.t -> (Meal.t, Error.t) result
val get_meal : t -> user:User.t -> Meal_id.t -> (Meal.t, Error.t) result

val query_meals :
  ?search:Search_query.t ->
  t ->
  user:User.t ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  limit:int ->
  (Meal.t list, Error.t) result

val update_meal :
  t -> user:User.t -> Meal_id.t -> Meal.Patch.t -> (Meal.t, Error.t) result

val delete_meal : t -> user:User.t -> Meal_id.t -> (unit, Error.t) result

val meals_between :
  t ->
  user:User.t ->
  start:Ptime.t ->
  stop:Ptime.t ->
  (Meal.t list, Error.t) result

val create_manual_weigh_in :
  t -> user:User.t -> Weigh_in.Create.t -> (Weigh_in.t, Error.t) result

val get_weigh_in :
  t -> user:User.t -> Weigh_in_id.t -> (Weigh_in.t, Error.t) result

val query_weigh_ins :
  t ->
  user:User.t ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  limit:int ->
  (Weigh_in.t list, Error.t) result

val update_manual_weigh_in :
  t ->
  user:User.t ->
  Weigh_in_id.t ->
  Weigh_in.Patch.t ->
  (Weigh_in.t, Error.t) result

val delete_weigh_in :
  t -> user:User.t -> Weigh_in_id.t -> (unit, Error.t) result

val weigh_ins_between :
  t ->
  user:User.t ->
  start:Ptime.t ->
  stop:Ptime.t ->
  (Weigh_in.t list, Error.t) result

val latest_weigh_in : t -> user:User.t -> (Weigh_in.t, Error.t) result

val create_withings_oauth_state :
  t ->
  user:User.t ->
  state_hash:string ->
  expires_at:Ptime.t ->
  (unit, Error.t) result

val consume_withings_oauth_state :
  t -> user:User.t -> state_hash:string -> now:Ptime.t -> (unit, Error.t) result

val get_withings_status :
  t -> user:User.t -> (Withings_connection.status, Error.t) result

val replace_withings_connection :
  t ->
  user:User.t ->
  key:Secret.Key.t ->
  Withings.credentials ->
  (Withings_connection.connected, Error.t) result

val refresh_withings_credentials :
  t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  key:Secret.Key.t ->
  Withings.credentials ->
  (Withings_connection.connected, Error.t) result

val withings_credentials :
  t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  key:Secret.Key.t ->
  (Withings.credentials, Error.t) result

val mark_withings_reauthorization :
  t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  (unit, Error.t) result

val persist_withings_import :
  t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  rows:Weigh_in.Import.t list ->
  expected_cursor:int64 option ->
  cursor:int64 option ->
  (unit, Error.t) result

val open_database : path:string -> (t, Error.t) result
val close : t -> (unit, Error.t) result
val delete_withings_connection : t -> user:User.t -> (unit, Error.t) result

val withings_connection_for_identity :
  t ->
  withings_user_id:string ->
  (User.t * Withings_connection.connected, Error.t) result

val withings_connections :
  t -> ((User.t * Withings_connection.connected) list, Error.t) result

val consume_withings_oauth_state_for_callback :
  t -> state_hash:string -> now:Ptime.t -> (User.t, Error.t) result

module For_testing : sig
  val raw_database : t -> Sqlite3.db
end

val compile_search_query : t -> string list -> (Search_query.t, Error.t) result
