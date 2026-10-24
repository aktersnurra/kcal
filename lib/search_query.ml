type t = string

(* Fold Swedish letters as well as ASCII before deduplication, so alternative
   spellings differing only in case cannot give these words extra BM25 weight.
   Other UTF-8 text is preserved; unicode61 handles matching case. *)
let fold_swedish text =
  String.mapi (fun i c ->
    if i > 0 && text.[i - 1] = '\195' then
      match c with '\132' -> '\164' | '\133' -> '\165' | '\150' -> '\182' | _ -> c
    else c) text

let words text =
  let text = fold_swedish text in
  let normalized = String.map (fun c ->
    match c with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' -> Char.lowercase_ascii c
    | c when Char.code c >= 128 -> c
    | _ -> ' ') text in
  String.split_on_char ' ' normalized |> List.filter (fun word -> word <> "")

let of_strings alternatives =
  let invalid () = Error (Error.Invalid_input "search requires nonblank alternatives with searchable words") in
  let groups = List.map words alternatives in
  if groups = [] || List.exists (fun group -> group = []) groups then invalid ()
  else
    let words = List.concat groups |> List.sort_uniq String.compare in
    Ok (String.concat " OR " (List.map (fun word -> "\"" ^ word ^ "\"") words))

let fts5_match_expression query = query
