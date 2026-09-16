type t = { store : Store_sqlite.t }

let make ~store = { store }

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

let record_manual_weigh_in service ~user input =
  Store_sqlite.create_manual_weigh_in service.store ~user input

let get_weigh_in service ~user id = Store_sqlite.get_weigh_in service.store ~user id

let query_weigh_ins service ~user ~from ~to_ ~limit =
  Store_sqlite.query_weigh_ins service.store ~user ~from ~to_ ~limit

let update_manual_weigh_in service ~user id patch =
  Store_sqlite.update_manual_weigh_in service.store ~user id patch

let delete_weigh_in service ~user id =
  Store_sqlite.delete_weigh_in service.store ~user id
