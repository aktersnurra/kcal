module Imported_identity : sig
  type t = private { account_id : string; group_id : string }

  val of_external_id : string -> (t, Error.t) result
  val external_id : t -> string
end

type source = Manual | Withings of Imported_identity.t
type manual_create = { measured_at : Ptime.t option; weight_kg : float }
type patch = { measured_at : Ptime.t option; weight_kg : float option }
type import = { external_id : string; measured_at : Ptime.t; weight_kg : float }

type t = private {
  id : Weigh_in_id.t;
  user_id : User_id.t;
  measured_at : Ptime.t;
  weight_kg : float;
  source : source;
  external_id : string option;
  created_at : Ptime.t;
  updated_at : Ptime.t;
  deleted_at : Ptime.t option;
}

module Create : sig
  type raw = manual_create
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

module Patch : sig
  type raw = patch
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

module Import : sig
  type raw = import
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

val of_storage :
  id:Weigh_in_id.t ->
  user_id:User_id.t ->
  measured_at:Ptime.t ->
  weight_kg:float ->
  source:source ->
  created_at:Ptime.t ->
  updated_at:Ptime.t ->
  deleted_at:Ptime.t option ->
  (t, Error.t) result

val source_to_string : source -> string
val empty_patch : patch
