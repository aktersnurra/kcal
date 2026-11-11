val save_product :
  Sqlite_db.t ->
  user:User.t ->
  id:Product_id.t option ->
  Product.Create.t ->
  (Product.t, Error.t) result

val search_products :
  Sqlite_db.t ->
  user:User.t ->
  query:Search_query.t ->
  limit:int ->
  (Product.t list, Error.t) result

val get_product :
  Sqlite_db.t -> user:User.t -> Product_id.t -> (Product.t, Error.t) result

val recent_products :
  Sqlite_db.t ->
  user:User.t ->
  since:Ptime.t ->
  limit:int ->
  ((Product.t * Ptime.t) list, Error.t) result
(** With when each was last eaten. *)
