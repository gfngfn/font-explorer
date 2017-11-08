
let arg_spec_list = []

let font_file_loaded_ref = ref false

let handle_input s =
  font_file_loaded_ref := true;
(*
  let ic = open_in s in
*)
  ()

let () =
  Arg.parse arg_spec_list handle_input "";
  if not !font_file_loaded_ref then
    begin
      print_endline "No font file specified.";
      exit 1;
    end
  else
    Terminal.show_list 2 ["The"; "quick"; "brown"; "fox"; "jumps"]
