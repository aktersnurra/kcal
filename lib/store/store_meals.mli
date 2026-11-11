type new_item = Meal_item.Create.t * Meal_item.product_link option
type idempotency = Meal.idempotency = { key : string; fingerprint : string }

val idempotent_meal :
  Sqlite_db.t -> user:User.t -> idempotency -> (Meal.t option, Error.t) result
(** The meal a key already recorded, [None] when the key is unused; rejected for
    a deleted meal or different content. *)

val create_meal :
  ?idempotency:idempotency ->
  Sqlite_db.t ->
  user:User.t ->
  eaten_at:Ptime.t option ->
  label:string option ->
  new_item list ->
  (Meal.t * [ `Recorded | `Replayed ], Error.t) result
(** Rejects an empty item list. A retry with the same key and content returns
    the meal already recorded; see [idempotent_meal]. *)

val get_meal :
  Sqlite_db.t -> user:User.t -> Meal_id.t -> (Meal.t, Error.t) result

val get_meal_with_revisions :
  Sqlite_db.t ->
  user:User.t ->
  Meal_id.t ->
  (Meal.t * (Meal_item_id.t * Meal_item.revision) list, Error.t) result
(** Revisions per item, oldest first. *)

val query_meals :
  ?search:Search_query.t ->
  Sqlite_db.t ->
  user:User.t ->
  from:Ptime.t option ->
  to_:Ptime.t option ->
  limit:int ->
  (Meal.t list, Error.t) result
(** A search matches meals with a matching live item, ranked by their best. *)

val update_meal :
  Sqlite_db.t ->
  user:User.t ->
  Meal_id.t ->
  Meal.Patch.t ->
  (Meal.t, Error.t) result
(** The meal's time and label. *)

val delete_meal :
  Sqlite_db.t -> user:User.t -> Meal_id.t -> (unit, Error.t) result

val add_item :
  Sqlite_db.t ->
  user:User.t ->
  Meal_id.t ->
  new_item ->
  (Meal.t, Error.t) result

val update_item :
  ?reason:string ->
  ?quantity:float ->
  Sqlite_db.t ->
  user:User.t ->
  Meal_item_id.t ->
  Meal_item.Patch.t ->
  (Meal.t, Error.t) result
(** Records the item as it was in a revision, in the same transaction. A new
    [quantity] rescales a product-linked item from its own values and keeps the
    link; a hand-edited nutrition value detaches it. *)

val delete_item :
  Sqlite_db.t -> user:User.t -> Meal_item_id.t -> (Meal.t, Error.t) result
(** Rejected for a meal's last item. *)

val meals_between :
  Sqlite_db.t ->
  user:User.t ->
  start:Ptime.t ->
  stop:Ptime.t ->
  (Meal.t list, Error.t) result

val items_from_earlier_versions :
  Sqlite_db.t ->
  user:User.t ->
  Product.t ->
  ((Meal_id.t * Ptime.t * Meal_item.t) list, Error.t) result
(** With each item's meal and when it was eaten. *)

val rescale_items :
  Sqlite_db.t ->
  reason:string ->
  (Meal_id.t * Meal_item.t * Product.portion * Meal_item.product_link) list ->
  (unit, Error.t) result
