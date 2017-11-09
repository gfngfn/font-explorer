
open Result

type error =
  | OtfmError   of Otfm.error
  | Bug
  | Message     of string
  | SystemError of string

let ( >>= ) (type a) (type b) (type c) (x : (a, c) result) (f : a -> (b, c) result) : (b, c) result =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e)

let ( >>=@ ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(OtfmError(e))

let return v = Ok(v)

let err e = Error(e)

let report_result (r : (unit, error) result) : unit =
  match r with
  | Ok(())   -> ()
  | Error(e) ->
      match e with
      | OtfmError(oe)    -> Otfm.pp_error Format.std_formatter oe
      | Bug              -> print_endline "bug"
      | Message(msg)     -> print_endline msg
      | SystemError(msg) -> print_endline msg


let arg_spec_list = []

let font_file_loaded_ref = ref false


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


let string_of_file fontfile : (string, error) result =
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
      err Bug
    with
    | Exit              -> close_in ic; return (Buffer.contents b)
    | Failure(_)        -> close_in ic; err (Message(Printf.sprintf "%s: too large input" fontfile))
    | Sys_error(errmsg) -> close_in ic; err (SystemError(errmsg))
  with
  | Sys_error(errmsg) -> err (Message(errmsg))


let handle_input fontfile =
  font_file_loaded_ref := true;
  let res =
    begin
      string_of_file fontfile >>= fun s ->
      let dcdr = Otfm.decoder (`String(s)) in
      Otfm.table_list dcdr >>=@ fun taglst ->
        begin
          Terminal.initialize ();
          try
            begin
              loop None 1 (List.map Otfm.Tag.to_bytes taglst);
              Terminal.terminate ();
              return ()
            end
          with
          | Assert_failure(_, _, _) ->
              begin
                Terminal.terminate ();
                err (Message("internal error"))
              end
        end
    end
  in
    report_result res


let () =
  Arg.parse arg_spec_list handle_input "";
  if not !font_file_loaded_ref then
    begin
      prerr_endline "No font file specified.";
      exit 1;
    end
  else ()
