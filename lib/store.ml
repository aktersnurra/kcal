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
end
