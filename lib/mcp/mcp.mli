val rpc_error :
  ?id:Yojson.Safe.t -> ?data:Yojson.Safe.t -> int -> string -> Yojson.Safe.t

val handle :
  ?withings:Withings_integration.t ->
  service:Service.t ->
  identity:Identity.t ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, [ `Forbidden ]) result
