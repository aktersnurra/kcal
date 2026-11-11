val sync_all :
  store:Store_sqlite.t ->
  sync:
    (user:User.t ->
    connection:Withings_connection.connected ->
    (unit, Error.t) result) ->
  (unit, Error.t) result
