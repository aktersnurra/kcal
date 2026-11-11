type t = Sqlite_db.t

include Store_users
include Store_products
include Store_meals
include Store_weigh_ins
include Store_withings
include Store_goals

let open_database ~path =
  Sqlite_db.open_database ~path ~migrate:Migration.apply_all

let close = Sqlite_db.close
let compile_search_query db = Search_query.of_strings (Sqlite_db.raw db)

module For_testing = struct
  let raw_database = Sqlite_db.raw
  let cached_statement_count = Sqlite_db.cached_statement_count
end
