type t

val make :
  store:Store_sqlite.t ->
  client:(module Withings.S) ->
  token_key:Secret.Key.t ->
  now:(unit -> Ptime.t) ->
  t

val imports :
  Withings_connection.Account.t ->
  Withings.measurement list ->
  (Weigh_in.Import.t list, Error.t) result

val sync :
  t ->
  user:User.t ->
  connection:Withings_connection.connected ->
  (unit, Error.t) result
