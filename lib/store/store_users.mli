val resolve_user :
  Sqlite_db.t -> issuer:string -> subject:string -> (User.t, Error.t) result
