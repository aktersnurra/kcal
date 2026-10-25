module Account : sig
  type t

  val to_string : t -> string
end

type t = {
  id : string;
  user_id : User_id.t;
  withings_user_id : string option;
  token_expires_at : Ptime.t option;
  sync_cursor : int64 option;
  requires_reauthorization : bool;
}

type status =
  | Connected of {
      withings_user_id : string option;
      token_expires_at : Ptime.t option;
      requires_reauthorization : bool;
    }

val status : t -> status
val connected_account : t -> (Account.t, Error.t) result
