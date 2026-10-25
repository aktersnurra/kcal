let () =
  print_endline "let all = [";
  Array.iteri
    (fun index path ->
      if index > 0 then
        let channel = open_in_bin path in
        let sql =
          Fun.protect
            ~finally:(fun () -> close_in channel)
            (fun () -> really_input_string channel (in_channel_length channel))
        in
        let version = int_of_string (String.sub (Filename.basename path) 0 3) in
        Printf.printf "(%d, %S);\n" version sql)
    Sys.argv;
  print_endline "]"
