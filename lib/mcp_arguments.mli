type range_request = { zone : Time.zone; range : Date_range.t }

type summary_request = {
  range_request : range_request;
  calorie_target : Summary.Calorie_target.t option;
  protein_target : Summary.Protein_target.t option;
}

type query_request = {
  from : Ptime.t option;
  to_ : Ptime.t option;
  limit : int;
}

type request = private
  | Begin_withings_connection
  | Get_withings_status
  | Disconnect_withings
  | Save_product of Product_id.t option * Product.Create.t
  | Search_products of Search_query.t * int
  | Record_meal of Meal.Create.t
  | Get_meal of Meal_id.t
  | Query_meals of Search_query.t option * query_request
  | Get_daily_totals of range_request
  | Get_daily_series of range_request
  | Get_summary of summary_request
  | Update_meal of Meal_id.t * Meal.Patch.t
  | Delete_meal of Meal_id.t
  | Record_weight of Weigh_in.Create.t
  | Get_weight of Weigh_in_id.t
  | Query_weights of query_request
  | Get_latest_weight
  | Update_weight of Weigh_in_id.t * Weigh_in.Patch.t
  | Delete_weight of Weigh_in_id.t

val parse :
  now:Ptime.t ->
  compile_search:(string list -> (Search_query.t, Error.t) result) ->
  name:string ->
  Yojson.Safe.t ->
  (request, Error.t) result
