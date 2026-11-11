type 'a t = Keep | Clear | Set of 'a

let apply current = function
  | Keep -> current
  | Clear -> None
  | Set value -> Some value
