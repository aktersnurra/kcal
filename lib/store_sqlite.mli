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

type new_item = Meal_item.Create.t * Meal_item.product_link option

type idempotency = { key : string; fingerprint : string }
(** [fingerprint] identifies the request content, so a reused key with other
    content is told apart from a retry. *)

val idempotent_meal :
  t -> user:User.t -> idempotency -> (Meal.t option, Error.t) result
(** The meal a key already recorded, [None] when the key is unused; rejected for
    a deleted meal or different content. *)

val create_meal :
  ?idempotency:idempotency ->
  t ->
  user:User.t ->
  eaten_at:Ptime.t option ->
  label:string option ->
  new_item list ->
  (Meal.t * [ `Recorded | `Replayed ], Error.t) result
(** Rejects an empty item list. A retry with the same key and content returns
    the meal already recorded; see [idempotent_meal]. *)

val get_meal : t -> user:User.t -> Meal_id.t -> (Meal.t, Error.t) result

val get_meal_with_revisions :
  t ->
  user:User.t ->
  Meal_id.t ->
  (Meal.t * (Meal_item_id.t * Meal_item.revision) list, Error.t) result
(** Revisions per item, oldest first. *)

val query_meals :
  ?search:Search_query.t ->
  t ->
  user:User.t ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  limit:int ->
  (Meal.t list, Error.t) result
(** A search matches meals with a matching live item, ranked by their best. *)

val update_meal :
  t -> user:User.t -> Meal_id.t -> Meal.Patch.t -> (Meal.t, Error.t) result
(** The meal's time and label. *)

val delete_meal : t -> user:User.t -> Meal_id.t -> (unit, Error.t) result

val add_item :
  t -> user:User.t -> Meal_id.t -> new_item -> (Meal.t, Error.t) result

val update_item :
  ?reason:string ->
  ?quantity:float ->
  t ->
  user:User.t ->
  Meal_item_id.t ->
  Meal_item.Patch.t ->
  (Meal.t, Error.t) result
(** Records the item as it was in a revision, in the same transaction. A new
    [quantity] rescales a product-linked item from its own values and keeps the
    link; a hand-edited nutrition value detaches it. *)

val delete_item : t -> user:User.t -> Meal_item_id.t -> (Meal.t, Error.t) result
(** Rejected for a meal's last item. *)

val meals_between :
  t ->
  user:User.t ->
  start:Ptime.t ->
  stop:Ptime.t ->
  (Meal.t list, Error.t) result

val items_from_earlier_versions :
  t ->
  user:User.t ->
  Product.t ->
  ((Meal_id.t * Ptime.t * Meal_item.t) list, Error.t) result
(** With each item's meal and when it was eaten. *)

val rescale_items :
  t ->
  reason:string ->
  (Meal_id.t * Meal_item.t * Product.portion * Meal_item.product_link) list ->
  (unit, Error.t) result

val get_product :
  t -> user:User.t -> Product_id.t -> (Product.t, Error.t) result

val recent_products :
  t ->
  user:User.t ->
  since:Ptime.t ->
  limit:int ->
  ((Product.t * Ptime.t) list, Error.t) result
(** With when each was last eaten. *)

val get_goals : t -> user:User.t -> (Goals.t, Error.t) result
val set_goals : t -> user:User.t -> Goals.t -> (Goals.t, Error.t) result

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
