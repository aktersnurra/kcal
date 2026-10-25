type t = private {
  id : Product_id.t;
  user_id : User_id.t;
  name : string;
  basis : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  source : string;
  created_at : Ptime.t;
  updated_at : Ptime.t;
}

type input = {
  name : string;
  basis : string;
  calories_kcal : int;
  protein_g : float;
  carbs_g : float option;
  fat_g : float option;
  source : string;
}

module Create : sig
  type raw = input
  type t

  val of_raw : raw -> (t, Error.t) result
  val to_raw : t -> raw
end

val of_storage :
  id:Product_id.t ->
  user_id:User_id.t ->
  name:string ->
  basis:string ->
  calories_kcal:int ->
  protein_g:float ->
  carbs_g:float option ->
  fat_g:float option ->
  source:string ->
  created_at:Ptime.t ->
  updated_at:Ptime.t ->
  (t, Error.t) result
