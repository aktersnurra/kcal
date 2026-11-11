type t = {
  id : User_id.t;
  oidc_issuer : string;
  oidc_subject : string;
  created_at : Ptime.t;
}
