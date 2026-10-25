type t

val make : store:Store_sqlite.t -> t

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

val latest_weigh_in : t -> user:User.t -> (Weigh_in.t, Error.t) result

val withings_connection_for_identity :
  t ->
  withings_user_id:string ->
  (User.t * Withings_connection.connected, Error.t) result

val compile_search_query : t -> string list -> (Search_query.t, Error.t) result

val begin_withings_authorization :
  client_id:string ->
  redirect_uri:string ->
  oauth:Withings_oauth.t ->
  user:User.t ->
  (string * string, Error.t) result

val daily_series :
  t ->
  user:User.t ->
  zone:Time.zone ->
  range:Date_range.t ->
  (Summary.day list, Error.t) result

val summary :
  ?calorie_target:Summary.Calorie_target.t ->
  ?protein_target:Summary.Protein_target.t ->
  t ->
  user:User.t ->
  zone:Time.zone ->
  range:Date_range.t ->
  (Summary.t, Error.t) result

val connect_withings :
  t ->
  user:User.t ->
  key:Secret.Key.t ->
  Withings.credentials ->
  (Withings_connection.connected, Error.t) result

val disconnect_withings : t -> user:User.t -> (unit, Error.t) result

val withings_status :
  t -> user:User.t -> (Withings_connection.status, Error.t) result

val record_meal : t -> user:User.t -> Meal.Create.t -> (Meal.t, Error.t) result

val record_manual_weigh_in :
  t -> user:User.t -> Weigh_in.Create.t -> (Weigh_in.t, Error.t) result

type withings_authorization_failure =
  | Invalid_state of Error.t
  | Exchange_failed of Error.t
  | Persistence_failed of Error.t
  | Sync_failed of Error.t
  | Subscription_failed of Error.t

val complete_withings_authorization :
  t ->
  oauth:Withings_oauth.t ->
  key:Secret.Key.t ->
  client:(module Withings.S) ->
  redirect_uri:string ->
  callback_url:string ->
  sync:(User.t -> Withings_connection.connected -> (unit, Error.t) result) ->
  state:string ->
  code:string ->
  (unit, withings_authorization_failure) result
