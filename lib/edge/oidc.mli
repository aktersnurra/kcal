type claims = {
  issuer : string;
  subject : string;
  audience : string list;
  expires_at : Ptime.t;
  scopes : string list;
}

type clock = unit -> Ptime.t
type discovery = { issuer : string; jwks_uri : string; max_age_s : int option }
type jwks_response = { keys : Jose.Jwks.t; max_age_s : int option }

type client = {
  discover : issuer:string -> (discovery, unit) result;
  fetch_jwks : uri:string -> (jwks_response, unit) result;
}

type t = string -> (claims, unit) result

val make :
  issuer:string -> audience:string -> ?clock:clock -> client:client -> unit -> t

(** Verifies a bearer token against the issuer's published keys. The keys are
    cached for the lifetime the provider states, at most an hour. A token signed
    with a key id the cache lacks triggers one refetch, at most once a minute.
    While the provider cannot be reached, expired keys stay trusted for up to a
    day and the fetch is retried every thirty seconds. Concurrent requests share
    one fetch. *)

val https_client :
  < net : _ Eio.Net.t ; clock : _ Eio.Time.clock ; .. > -> client
(** Each request is abandoned after [Https_transport.request_timeout_s]. *)

val of_verified_claims :
  ?issuer:string -> ?audience:string -> ?clock:clock -> t -> t

module For_testing : sig
  val client_of_get :
    (string -> (string * Cohttp.Header.t, unit) result) -> client
  (** Injects document fetching while retaining document decoding and transport
      errors. *)
end
