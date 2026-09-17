module type S = sig
  type t

  val resolve_user : t -> issuer:string -> subject:string -> (User.t, Error.t) result
  val create_meal : t -> user:User.t -> Meal.create -> (Meal.t, Error.t) result
  val get_meal : t -> user:User.t -> Meal_id.t -> (Meal.t, Error.t) result

  val query_meals :
    t ->
    user:User.t ->
    from:Ptime.t option ->
    to_:Ptime.t option ->
    limit:int ->
    (Meal.t list, Error.t) result

  val update_meal : t -> user:User.t -> Meal_id.t -> Meal.patch -> (Meal.t, Error.t) result
  val delete_meal : t -> user:User.t -> Meal_id.t -> (unit, Error.t) result

  val create_manual_weigh_in :
    t -> user:User.t -> Weigh_in.manual_create -> (Weigh_in.t, Error.t) result
  val get_weigh_in : t -> user:User.t -> Weigh_in_id.t -> (Weigh_in.t, Error.t) result

  val query_weigh_ins :
    t ->
    user:User.t ->
    from:Ptime.t option ->
    to_:Ptime.t option ->
    limit:int ->
    (Weigh_in.t list, Error.t) result

  val update_manual_weigh_in :
    t -> user:User.t -> Weigh_in_id.t -> Weigh_in.patch -> (Weigh_in.t, Error.t) result
  val delete_weigh_in : t -> user:User.t -> Weigh_in_id.t -> (unit, Error.t) result

  val create_withings_oauth_state :
    t -> user:User.t -> state_hash:string -> expires_at:Ptime.t -> (unit, Error.t) result

  val consume_withings_oauth_state :
    t -> user:User.t -> state_hash:string -> now:Ptime.t -> (unit, Error.t) result

  val create_withings_connection :
    t -> user:User.t -> withings_user_id:string -> (Withings_connection.t, Error.t) result

  val get_withings_status :
    t -> user:User.t -> (Withings_connection.status, Error.t) result
end
