
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


let rec loop rowhl =
  Terminal.show_list rowhl lst;
  assert (Curses.mvaddstr 10 2 "waiting input...");
  let c = Curses.getch () in
  match Char.chr c with
  | 'n' ->
      let rowhlnew =
        if rowhl >= List.length lst then rowhl else rowhl + 1
      in
        loop rowhlnew

  | 'p' ->
      let rowhlnew =
        if rowhl <= 1 then rowhl else rowhl - 1
      in
        loop rowhlnew

  | _ -> ()


let () =
  Arg.parse arg_spec_list handle_input "";
  if not !font_file_loaded_ref then
    begin
      prerr_endline "No font file specified.";
      exit 1;
    end
  else
    let _ = Curses.initscr () in
    begin
      assert (Curses.cbreak ());
      assert (Curses.noecho ());
      loop 1;
      Curses.endwin ();
    end
