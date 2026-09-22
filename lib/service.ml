type t = { store : Store_sqlite.t; withings_oauth : Withings_oauth.t }

let make ~store =
  let now () = Option.get (Ptime.of_float_s (Unix.gettimeofday ())) in
  { store; withings_oauth = Withings_oauth.make ~store ~now }

let resolve_user service ~issuer ~subject =
  Store_sqlite.resolve_user service.store ~issuer ~subject

let record_meal service ~user input =
  Store_sqlite.create_meal service.store ~user input

let get_meal service ~user id = Store_sqlite.get_meal service.store ~user id

let query_meals service ~user ~from ~to_ ~limit =
  Store_sqlite.query_meals service.store ~user ~from ~to_ ~limit

let update_meal service ~user id patch =
  Store_sqlite.update_meal service.store ~user id patch

let delete_meal service ~user id = Store_sqlite.delete_meal service.store ~user id

let daily_totals service ~user ~day_start ~day_end =
  Store_sqlite.daily_totals service.store ~user ~day_start ~day_end

let record_manual_weigh_in service ~user input =
  Store_sqlite.create_manual_weigh_in service.store ~user input

let get_weigh_in service ~user id = Store_sqlite.get_weigh_in service.store ~user id

let query_weigh_ins service ~user ~from ~to_ ~limit =
  Store_sqlite.query_weigh_ins service.store ~user ~from ~to_ ~limit

let update_manual_weigh_in service ~user id patch =
  Store_sqlite.update_manual_weigh_in service.store ~user id patch

let delete_weigh_in service ~user id =
  Store_sqlite.delete_weigh_in service.store ~user id

let latest_weigh_in service ~user = Store_sqlite.latest_weigh_in service.store ~user

let begin_withings_authorization ~client_id ~redirect_uri service ~user =
  Withings_oauth.begin_authorization ~client_id ~redirect_uri service.withings_oauth ~user

let consume_withings_authorization_state service ~user ~state =
  Withings_oauth.consume_state service.withings_oauth ~user ~state

let get_withings_status service ~user =
  Store_sqlite.get_withings_status service.store ~user
