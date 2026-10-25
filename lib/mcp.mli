type withings = {
  oauth : Withings_oauth.t;
  client_id : string;
  redirect_uri : string;
}

val rpc_error : ?id:Yojson.Safe.t -> int -> string -> Yojson.Safe.t

val handle :
  ?withings:withings ->
  service:Service.t ->
  identity:Auth.identity ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, [ `Forbidden ]) result
