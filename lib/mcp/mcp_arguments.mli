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

(** A parsed tool call. ['search] is whatever the caller's [compile_search]
    makes of search text, so parsing does not depend on how search is stored. *)
type 'search request = private
  | Begin_withings_connection
  | Get_withings_status
  | Disconnect_withings
  | Get_context
  | Set_goals of Goals.patch
  | Save_product of Product_id.t option * Product.Create.t
  | Search_products of 'search * int
  | Correct_meals_from_product of product_correction
  | Record_meal of {
      idempotency : Meal.idempotency option;
      eaten_at : Ptime.t option;
      label : string option;
      items : Meal_item.entry list;
    }
  | Get_meal of Meal_id.t
  | Query_meals of 'search option * query_request
  | Get_daily_series of range_request
  | Get_summary of range_request
  | Update_meal of Meal_id.t * Meal.Patch.t
  | Delete_meal of Meal_id.t
  | Add_meal_item of Meal_id.t * Meal_item.entry
  | Update_meal_item of {
      id : Meal_item_id.t;
      patch : Meal_item.Patch.t;
      reason : string option;
      quantity : float option;
    }
  | Delete_meal_item of Meal_item_id.t
  | Record_weight of Weigh_in.Create.t
  | Get_weight of Weigh_in_id.t
  | Query_weights of query_request
  | Update_weight of Weigh_in_id.t * Weigh_in.Patch.t
  | Delete_weight of Weigh_in_id.t

(** The tools kcal offers. A tool exists only as a constructor here, so the
    compiler makes every match over them, such as the scope each one requires,
    account for a newly added tool. *)
module Tool : sig
  type t =
    | Save_product
    | Search_products
    | Correct_meals_from_product
    | Record_meal
    | Get_meal
    | Query_meals
    | Update_meal
    | Add_meal_item
    | Update_meal_item
    | Delete_meal_item
    | Delete_meal
    | Record_weight
    | Get_weight
    | Query_weights
    | Update_weight
    | Delete_weight
    | Begin_withings_connection
    | Get_withings_status
    | Disconnect_withings
    | Get_context
    | Set_goals
    | Get_daily_series
    | Get_summary

  val all : t list
  val name : t -> string

  val of_name : string -> t option
  (** [None] for a name that is not a tool. *)
end

val parse :
  now:Ptime.t ->
  default_zone:(unit -> (Time.zone, Error.t) result) ->
  compile_search:(string list -> ('search, Error.t) result) ->
  name:string ->
  Yojson.Safe.t ->
  ('search request, Error.t) result
(** [name] that is not a tool is rejected. [default_zone] is consulted only when
    a calendar tool omits [timezone]. *)
