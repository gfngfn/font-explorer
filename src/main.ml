
let arg_spec_list = []

let font_file_loaded_ref = ref false

let handle_input s =
  font_file_loaded_ref := true;
(*
  let ic = open_in s in
*)
  ()

let lst =
  ["The"; "quick"; "brown"; "fox"; "jumps"]


let rec loop errcopt rowhl =
  Terminal.show_list rowhl lst;
  Terminal.print_status "waiting input...";
  let () =
    match errcopt with
    | None    -> ()
    | Some(c) -> Terminal.print_error (Printf.sprintf "[%d]" c)
  in
  let c = Curses.getch () in
  match Char.chr c with
  | 'n' ->
      let rowhlnew =
        if rowhl >= List.length lst then rowhl else rowhl + 1
      in
        loop None rowhlnew

  | 'p' ->
      let rowhlnew =
        if rowhl <= 1 then rowhl else rowhl - 1
      in
        loop None rowhlnew

  | 'q' -> ()

  | _ ->
        loop (Some(c)) rowhl


let () =
  Arg.parse arg_spec_list handle_input "";
  if not !font_file_loaded_ref then
    begin
      prerr_endline "No font file specified.";
      exit 1;
    end
  else
    begin
      Terminal.initialize ();
      try
        begin
          loop None 1;
          Terminal.terminate ();
        end
      with
      | Assert_failure(_, _, _) ->
          begin
            Terminal.terminate ();
            print_endline "Internal error has occured.";
          end
    end
