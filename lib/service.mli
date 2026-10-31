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

val get_meal :
  t -> user:User.t -> Meal_id.t -> (Meal.t * Meal.revision list, Error.t) result
(** With its revisions, oldest first. *)

val query_meals :
  ?search:Search_query.t ->
  t ->
  user:User.t ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  limit:int ->
  (Meal.t list, Error.t) result

val update_meal :
  ?reason:string ->
  t ->
  user:User.t ->
  Meal_id.t ->
  Meal.Patch.t ->
  (Meal.t, Error.t) result

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

type summary = {
  current : Summary.t;
  previous : (Date_range.t * Summary.t) option;
      (** The equally long range just before, when it is representable. *)
}

val summary :
  t ->
  user:User.t ->
  zone:Time.zone ->
  range:Date_range.t ->
  (summary, Error.t) result
(** Judged against the user's goals, when set. *)

val context : t -> user:User.t -> now:Ptime.t -> (Context.t, Error.t) result
val goals : t -> user:User.t -> (Goals.t option, Error.t) result
val set_goals : t -> user:User.t -> Goals.t -> (Goals.t, Error.t) result

val default_zone : t -> user:User.t -> (Time.zone, Error.t) result
(** The goals' timezone, or UTC without goals. *)

type product_correction = {
  product : Product.t;
  meals : (Meal.t * Product.portion) list;
      (** Each meal as it stands, with the nutrition the product now gives. *)
  applied : bool;
}

val correct_meals_from_product :
  t ->
  user:User.t ->
  Product_id.t ->
  reason:string ->
  dry_run:bool ->
  (product_correction, Error.t) result
(** Rescales live meals logged from an earlier version of the product to its
    current version, recording a revision with [reason] on each. *)

val connect_withings :
  t ->
  user:User.t ->
  key:Secret.Key.t ->
  Withings.credentials ->
  (Withings_connection.connected, Error.t) result

val disconnect_withings : t -> user:User.t -> (unit, Error.t) result

val withings_status :
  t -> user:User.t -> (Withings_connection.status, Error.t) result

val record_meal :
  ?idempotency_key:string ->
  t ->
  user:User.t ->
  Meal.Create.t ->
  (Meal.t, Error.t) result

val record_meal_from_product :
  ?idempotency_key:string ->
  t ->
  user:User.t ->
  Meal.from_product ->
  (Meal.t, Error.t) result
(** Scales the product's current version to the quantity and links the meal to
    it. *)

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
