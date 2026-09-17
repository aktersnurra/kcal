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

let status connection =
  Connected
    {
      withings_user_id = connection.withings_user_id;
      token_expires_at = connection.token_expires_at;
      requires_reauthorization = connection.requires_reauthorization;
    }
