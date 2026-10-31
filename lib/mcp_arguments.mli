type range_request = { zone : Time.zone; range : Date_range.t }

type query_request = {
  from : Ptime.t option;
  to_ : Ptime.t option;
  limit : int;
}

type product_correction = {
  product_id : Product_id.t;
  reason : string;
  dry_run : bool;
}

type request = private
  | Begin_withings_connection
  | Get_withings_status
  | Disconnect_withings
  | Get_context
  | Set_goals of Goals.t
  | Save_product of Product_id.t option * Product.Create.t
  | Search_products of Search_query.t * int
  | Correct_meals_from_product of product_correction
  | Record_meal of {
      idempotency_key : string option;
      eaten_at : Ptime.t option;
      label : string option;
      items : Meal_item.entry list;
    }
  | Get_meal of Meal_id.t
  | Query_meals of Search_query.t option * query_request
  | Get_daily_series of range_request
  | Get_summary of range_request
  | Update_meal of Meal_id.t * Meal.Patch.t
  | Delete_meal of Meal_id.t
  | Add_meal_item of Meal_id.t * Meal_item.entry
  | Update_meal_item of {
      id : Meal_item_id.t;
      patch : Meal_item.Patch.t;
      reason : string option;
    }
  | Delete_meal_item of Meal_item_id.t
  | Record_weight of Weigh_in.Create.t
  | Get_weight of Weigh_in_id.t
  | Query_weights of query_request
  | Update_weight of Weigh_in_id.t * Weigh_in.Patch.t
  | Delete_weight of Weigh_in_id.t

val parse :
  now:Ptime.t ->
  default_zone:(unit -> (Time.zone, Error.t) result) ->
  compile_search:(string list -> (Search_query.t, Error.t) result) ->
  name:string ->
  Yojson.Safe.t ->
  (request, Error.t) result
(** [default_zone] is consulted only when a calendar tool omits [timezone]. *)
