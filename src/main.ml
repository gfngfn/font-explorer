
open Result

exception Bug
exception OtherError of string


let err msg = raise (OtherError(msg))


let arg_spec_list = []

let font_file_loaded_ref = ref false
(*
let lst =
  ["The"; "quick"; "brown"; "fox"; "jumps"]
*)

let rec loop errcopt rowhl lst =
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
        loop None rowhlnew lst

  | 'p' ->
      let rowhlnew =
        if rowhl <= 1 then rowhl else rowhl - 1
      in
        loop None rowhlnew lst

  | 'q' -> ()

  | _ ->
        loop (Some(c)) rowhl lst


let string_of_file fontfile =
  try
    let ic = open_in_bin fontfile in
    let bufsize = 65536 in
    let b = Buffer.create bufsize in
    let s = Bytes.create bufsize in
    try
      while true do
        let c = input ic s 0 bufsize in
        if c = 0 then
          raise Exit
        else
          Buffer.add_substring b (Bytes.unsafe_to_string s) 0 c
      done;
      raise (Bug)
    with
    | Exit              -> close_in ic; Buffer.contents b
    | Failure(_)        -> close_in ic; err (Printf.sprintf "%s: too large input" fontfile)
    | Sys_error(errmsg) -> close_in ic; err errmsg
  with
  | Sys_error(errmsg) -> err errmsg


let handle_input fontfile =
  font_file_loaded_ref := true;
  try
    let s = string_of_file fontfile in
    let dcdr = Otfm.decoder (`String(s)) in
      match Otfm.table_list dcdr with
      | Error(_) ->
          begin
            print_endline "font format broken.";
            ()
          end

      | Ok(taglst) ->
      begin
        Terminal.initialize ();
        try
          begin
            loop None 1 (List.map Otfm.Tag.to_bytes taglst);
            Terminal.terminate ();
          end
        with
        | Assert_failure(_, _, _) ->
            begin
              Terminal.terminate ();
              print_endline "Internal error has happened.";
            end
      end
    with
    | Bug ->
        begin
          print_endline "Internal error has happened.";
        end

    | OtherError(errmsg) ->
        begin
          Terminal.terminate ();
          print_endline ("Error: " ^ errmsg);
        end


let () =
  Arg.parse arg_spec_list handle_input "";
  if not !font_file_loaded_ref then
    begin
      prerr_endline "No font file specified.";
      exit 1;
    end
  else ()
