type t = { store : Store_sqlite.t; withings_oauth : Withings_oauth.t }

let make ~store =
  let now () = Option.get (Ptime.of_float_s (Unix.gettimeofday ())) in
  { store; withings_oauth = Withings_oauth.make ~store ~now }

let resolve_user service ~issuer ~subject =
  Store_sqlite.resolve_user service.store ~issuer ~subject

let record_meal service ~user input =
  Store_sqlite.create_meal service.store ~user input

let get_meal service ~user id = Store_sqlite.get_meal service.store ~user id

(* The HTTP service runs in one Eio domain. Compilation is synchronous and
   non-yielding, so its connection-local TEMP work is serialized with store work.
   The existing store owner remains responsible for closing the connection. *)
let compile_search_query service alternatives =
  Store_sqlite.compile_search_query service.store alternatives

let query_meals ?search service ~user ~from ~to_ ~limit =
  Store_sqlite.query_meals ?search service.store ~user ~from ~to_ ~limit

let update_meal service ~user id patch =
  Store_sqlite.update_meal service.store ~user id patch

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

let begin_withings_authorization ~client_id ~redirect_uri service ~user =
  Withings_oauth.begin_authorization ~client_id ~redirect_uri
    service.withings_oauth ~user

let consume_withings_authorization_state service ~user ~state =
  Withings_oauth.consume_state service.withings_oauth ~user ~state

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
