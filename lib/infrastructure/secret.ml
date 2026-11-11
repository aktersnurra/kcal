module Domain_error = Error
open Base
module Error = Domain_error

module Key = struct
  type t = Mirage_crypto.AES.GCM.key

  let of_bytes bytes =
    if Bytes.length bytes <> 32 then
      Error (Error.Invalid_input "invalid encryption key")
    else Ok (Mirage_crypto.AES.GCM.of_secret (Bytes.to_string bytes))
end

let invalid_secret () = Error.Invalid_input "invalid encrypted secret"
let rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let encrypt ~key plaintext =
  Lazy.force rng_initialized;
  let nonce = Mirage_crypto_rng.generate 12 in
  let ciphertext =
    Mirage_crypto.AES.GCM.authenticate_encrypt ~key ~nonce plaintext
  in
  "v1:" ^ Base64.encode_string nonce ^ ":" ^ Base64.encode_string ciphertext

(* Base64 tolerates stray bits in the last character before the padding, so
   several texts decode to the same bytes. Only the one [encrypt] writes is
   accepted: a stored envelope has exactly one valid spelling. *)
let decode_canonical encoded =
  match Base64.decode encoded with
  | Ok decoded when String.equal (Base64.encode_string decoded) encoded ->
      Some decoded
  | Ok _ | Error _ -> None

let decrypt ~key envelope =
  match String.split envelope ~on:':' with
  | [ "v1"; encoded_nonce; encoded_ciphertext ] -> (
      match
        (decode_canonical encoded_nonce, decode_canonical encoded_ciphertext)
      with
      | Some nonce, Some ciphertext when String.length nonce = 12 -> (
          match
            Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce ciphertext
          with
          | Some plaintext -> Ok plaintext
          | None -> Error (invalid_secret ()))
      | _ -> Error (invalid_secret ()))
  | _ -> Error (invalid_secret ())
