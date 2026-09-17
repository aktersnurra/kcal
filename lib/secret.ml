let invalid_secret () = Error.Invalid_input "invalid encrypted secret"
let rng_initialized = lazy (Mirage_crypto_rng_unix.use_default ())

let key_of_bytes key =
  if Bytes.length key <> 32 then Error (Error.Invalid_input "invalid encryption key")
  else
    try Ok (Mirage_crypto.AES.GCM.of_secret (Bytes.to_string key))
    with Invalid_argument _ -> Error (Error.Invalid_input "invalid encryption key")

let encrypt ~key plaintext =
  match key_of_bytes key with
  | Error _ as error -> error
  | Ok key ->
      try
        Lazy.force rng_initialized;
        let nonce = Mirage_crypto_rng.generate 12 in
        let ciphertext = Mirage_crypto.AES.GCM.authenticate_encrypt ~key ~nonce plaintext in
        Ok ("v1:" ^ Base64.encode_string nonce ^ ":" ^ Base64.encode_string ciphertext)
      with _ -> Error (invalid_secret ())

let decrypt ~key envelope =
  match key_of_bytes key, String.split_on_char ':' envelope with
  | Error _ as error, _ -> error
  | Ok key, [ "v1"; encoded_nonce; encoded_ciphertext ] ->
      (match Base64.decode encoded_nonce, Base64.decode encoded_ciphertext with
      | Ok nonce, Ok ciphertext when String.length nonce = 12 ->
          (match Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce ciphertext with
          | Some plaintext -> Ok plaintext
          | None -> Error (invalid_secret ()))
      | _ -> Error (invalid_secret ()))
  | Ok _, _ -> Error (invalid_secret ())
