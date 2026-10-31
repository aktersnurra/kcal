module Domain_error = Error
open Base
module Error = Domain_error

type t = { store : Store_sqlite.t }

let make ~store = { store }

let resolve_user service ~issuer ~subject =
  Store_sqlite.resolve_user service.store ~issuer ~subject

let ( let* ) result f = Result.bind result ~f

let record_meal ?idempotency_key service ~user input =
  Store_sqlite.create_meal ?idempotency_key service.store ~user input

let record_meal_from_product ?idempotency_key service ~user
    (entry : Meal.from_product) =
  let* product =
    Store_sqlite.get_product service.store ~user entry.product_id
  in
  let* portion = Product.portion product ~quantity:entry.quantity in
  let* input =
    Meal.Create.of_raw
      {
        description = Option.value entry.description ~default:product.name;
        calories_kcal = portion.calories_kcal;
        protein_g = portion.protein_g;
        carbs_g = portion.carbs_g;
        fat_g = portion.fat_g;
        confidence = entry.confidence;
        estimate_source =
          Some (Option.value entry.estimate_source ~default:product.source);
        notes = entry.notes;
        eaten_at = entry.eaten_at;
      }
  in
  Store_sqlite.create_meal ?idempotency_key
    ~product:
      {
        product_id = product.id;
        product_version = product.version;
        quantity = entry.quantity;
      }
    service.store ~user input

let get_meal service ~user id =
  let* meal = Store_sqlite.get_meal service.store ~user id in
  let* revisions = Store_sqlite.meal_revisions service.store ~user id in
  Ok (meal, revisions)

(* The HTTP service runs in one Eio domain. Compilation is synchronous and
   non-yielding, so its connection-local TEMP work is serialized with store work.
   The existing store owner remains responsible for closing the connection. *)
let compile_search_query service alternatives =
  Store_sqlite.compile_search_query service.store alternatives

let query_meals ?search service ~user ~from ~to_ ~limit =
  Store_sqlite.query_meals ?search service.store ~user ~from ~to_ ~limit

let update_meal ?reason service ~user id patch =
  Store_sqlite.update_meal ?reason service.store ~user id patch

let delete_meal service ~user id =
  Store_sqlite.delete_meal service.store ~user id

let meals_between service ~user ~start ~stop =
  Store_sqlite.meals_between service.store ~user ~start ~stop

let weigh_ins_between service ~user ~start ~stop =
  Store_sqlite.weigh_ins_between service.store ~user ~start ~stop

let record_manual_weigh_in service ~user input =
  Store_sqlite.create_manual_weigh_in service.store ~user input

let get_weigh_in service ~user id =
  Store_sqlite.get_weigh_in service.store ~user id

let query_weigh_ins service ~user ~from ~to_ ~limit =
  Store_sqlite.query_weigh_ins service.store ~user ~from ~to_ ~limit

let update_manual_weigh_in service ~user id patch =
  Store_sqlite.update_manual_weigh_in service.store ~user id patch

let delete_weigh_in service ~user id =
  Store_sqlite.delete_weigh_in service.store ~user id

let latest_weigh_in service ~user =
  Store_sqlite.latest_weigh_in service.store ~user

let begin_withings_authorization ~client_id ~redirect_uri ~oauth ~user =
  Withings_oauth.begin_authorization ~client_id ~redirect_uri oauth ~user

let withings_status service ~user =
  Store_sqlite.get_withings_status service.store ~user

let save_product service ~user ~id input =
  Store_sqlite.save_product service.store ~user ~id input

let search_products service ~user ~query ~limit =
  Store_sqlite.search_products service.store ~user ~query ~limit

let connect_withings service ~user ~key credentials =
  Store_sqlite.replace_withings_connection service.store ~user ~key credentials

let disconnect_withings service ~user =
  Store_sqlite.delete_withings_connection service.store ~user

let withings_connection_for_identity service ~withings_user_id =
  Store_sqlite.withings_connection_for_identity service.store ~withings_user_id

let goals service ~user =
  match Store_sqlite.get_goals service.store ~user with
  | Ok goals -> Ok (Some goals)
  | Error Error.Not_found -> Ok None
  | Error _ as error -> error

let set_goals service ~user goals =
  Store_sqlite.set_goals service.store ~user goals

let default_zone service ~user =
  Result.map (goals service ~user) ~f:(function
    | Some (goals : Goals.t) -> goals.timezone
    | None -> Time.utc)

type product_correction = {
  product : Product.t;
  meals : (Meal.t * Product.portion) list;
      (** Each meal as it stands, with the nutrition the product now gives. *)
  applied : bool;
}

let correct_meals_from_product service ~user product_id ~reason ~dry_run =
  let* product = Store_sqlite.get_product service.store ~user product_id in
  let* stale =
    Store_sqlite.meals_from_earlier_versions service.store ~user product
  in
  let* changes =
    List.map stale ~f:(fun (meal : Meal.t) ->
        let* link =
          Result.of_option meal.product
            ~error:(Error.Storage_decoding "meal product link")
        in
        let* portion = Product.portion product ~quantity:link.quantity in
        Ok (meal, portion, Meal.{ link with product_version = product.version }))
    |> Result.all
  in
  let meals = List.map changes ~f:(fun (meal, portion, _) -> (meal, portion)) in
  if dry_run || List.is_empty changes then
    Ok { product; meals; applied = false }
  else
    let* (_ : Meal.t list) =
      Store_sqlite.rescale_meals service.store ~user ~reason changes
    in
    Ok { product; meals; applied = true }

let records_in_range service ~user ~zone ~range =
  let* start, stop = Date_range.utc_bounds zone range in
  let* meals = meals_between service ~user ~start ~stop in
  let* weigh_ins = weigh_ins_between service ~user ~start ~stop in
  Ok (meals, weigh_ins)

let daily_series service ~user ~zone ~range =
  let* meals, weigh_ins = records_in_range service ~user ~zone ~range in
  Summary.daily_series zone range ~meals ~weigh_ins

type summary = {
  current : Summary.t;
  previous : (Date_range.t * Summary.t) option;
      (** The equally long range just before, when it is representable. *)
}

let previous_range range =
  let days = Date_range.day_count range in
  match Date.add_days (Date_range.first range) (-days) with
  | Error _ -> None
  | Ok first -> (
      match Date.add_days (Date_range.first range) (-1) with
      | Error _ -> None
      | Ok last -> Result.ok (Date_range.make ~first ~last))

let summary service ~user ~zone ~range =
  let* goals = goals service ~user in
  let summarise range =
    let* meals, weigh_ins = records_in_range service ~user ~zone ~range in
    Summary.summarise ?goals zone range ~meals ~weigh_ins
  in
  let* current = summarise range in
  let* previous =
    match previous_range range with
    | None -> Ok None
    | Some range ->
        Result.map (summarise range) ~f:(fun summary -> Some (range, summary))
  in
  Ok { current; previous }

let recent_product_limit = 5

let context service ~user ~now =
  let* goals = goals service ~user in
  let zone =
    Option.value_map goals ~default:Time.utc ~f:(fun (goals : Goals.t) ->
        goals.timezone)
  in
  let* history = Context.history zone ~now in
  let* start, stop = Date_range.utc_bounds zone history in
  let* meals = meals_between service ~user ~start ~stop in
  let* weigh_ins = weigh_ins_between service ~user ~start ~stop in
  let* latest_weight =
    match latest_weigh_in service ~user with
    | Ok weight -> Ok (Some weight)
    | Error Error.Not_found -> Ok None
    | Error _ as error -> error
  in
  let* recent_products =
    Store_sqlite.recent_products service.store ~user ~since:start
      ~limit:recent_product_limit
  in
  Context.build ~now ~zone ~goals ~meals ~weigh_ins ~latest_weight
    ~recent_products

type withings_authorization_failure =
  | Invalid_state of Error.t
  | Exchange_failed of Error.t
  | Persistence_failed of Error.t
  | Sync_failed of Error.t
  | Subscription_failed of Error.t

let complete_withings_authorization service ~oauth ~key ~client ~redirect_uri
    ~callback_url ~sync ~state ~code =
  let module Client = (val client : Withings.S) in
  let* user =
    Result.map_error
      ~f:(fun error -> Invalid_state error)
      (Withings_oauth.consume_callback_state oauth ~state)
  in
  let* credentials =
    Result.map_error
      ~f:(fun error -> Exchange_failed error)
      (Client.exchange_code ~redirect_uri ~code)
  in
  let* connection =
    Result.map_error
      ~f:(fun error -> Persistence_failed error)
      (connect_withings service ~user ~key credentials)
  in
  let* () =
    Result.map_error ~f:(fun error -> Sync_failed error) (sync user connection)
  in
  let* () =
    Result.map_error
      ~f:(fun error -> Subscription_failed error)
      (Client.subscribe ~access_token:credentials.access_token ~callback_url)
  in
  Ok ()
