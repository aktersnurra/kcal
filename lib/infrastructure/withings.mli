type credentials = {
  access_token : string;
  refresh_token : string;
  expires_at : Ptime.t;
  withings_user_id : string;
}

type measurement = {
  group_id : string;
  measured_at : int64;
  measure_type : int;
  value : int64;
  unit_ : int;
}

type continuation = Complete | Continue of int64

type measurement_page = {
  measurements : measurement list;
  lastupdate : int64 option;
  continuation : continuation;
}

type measurement_batch = {
  measurements : measurement list;
  lastupdate : int64 option;
}

module type S = sig
  val exchange_code :
    redirect_uri:string -> code:string -> (credentials, Error.t) result

  val refresh : refresh_token:string -> (credentials, Error.t) result

  val get_measurements :
    access_token:string ->
    lastupdate:int64 option ->
    (measurement_batch, Error.t) result

  val subscribe :
    access_token:string -> callback_url:string -> (unit, Error.t) result
end

type request = {
  post_form :
    uri:string -> fields:(string * string) list -> (string, Error.t) result;
}

type config = { client_id : string; client_secret : string }

val credentials_of_response :
  now:Ptime.t -> ?refresh:bool -> string -> (credentials, Error.t) result

val measurements_of_response : string -> (measurement_page, Error.t) result

val make :
  now:(unit -> Ptime.t) -> request:request -> config:config -> (module S)
(** Fetches all continuations before returning. The final page's raw optional
    watermark is the batch watermark; the query baseline never replaces it. *)
