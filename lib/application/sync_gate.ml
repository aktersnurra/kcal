open Base

(* Per key in flight: whether another run was requested meanwhile. *)
type t = (string, bool ref) Hashtbl.t

let create () = Hashtbl.create (module String)

let request t ~key ~schedule run =
  match Hashtbl.find t key with
  | Some again -> again := true
  | None ->
      let again = ref false in
      Hashtbl.set t ~key ~data:again;
      schedule (fun () ->
          Exn.protect
            ~finally:(fun () -> Hashtbl.remove t key)
            ~f:(fun () ->
              let rec loop () =
                again := false;
                run ();
                if !again then loop ()
              in
              loop ()))
