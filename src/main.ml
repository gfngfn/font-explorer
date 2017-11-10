
open Result
open Tree


type main_error =
  [
    | `Bug
    | `Message     of string
    | `SystemError of string
  ]

type error = [ main_error | Otfm.error ]

let ( >>= ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e :> error)
(*
let ( >>=@ ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(OtfmError(e))
*)
let return v = Ok(v)

let err e = Error(e)

let report_result (r : (unit, error) result) : unit =
  match r with
  | Ok(())   -> ()
  | Error(e) ->
      match e with
      | #Otfm.error as oe -> Otfm.pp_error Format.std_formatter oe
      | `Bug              -> print_endline "bug"
      | `Message(msg)     -> print_endline msg
      | `SystemError(msg) -> print_endline msg


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
      err `Bug
    with
    | Exit              -> close_in ic; return (Buffer.contents b)
    | Failure(_)        -> close_in ic; err (`Message(Printf.sprintf "%s: too large input" fontfile))
    | Sys_error(errmsg) -> close_in ic; err (`SystemError(errmsg))
  with
  | Sys_error(errmsg) -> err (`Message(errmsg))


let table_list dcdr =
  let res =
    Otfm.table_list dcdr >>= fun taglst ->
    let tagstrlst = taglst |> List.map Otfm.Tag.to_bytes in
    let tagtrlst = tagstrlst |> List.map (fun tagstr -> Element(tagstr, Evaled([]))) in
      return tagtrlst
  in
  match res with
  | Ok(tagtrlst) -> tagtrlst
  | Error(_)     -> []  (* temporary *)
  

let tree_of_font_file fontfile =
  string_of_file fontfile >>= fun s ->
  Otfm.decoder (`String(s)) >>= function
  | Otfm.TrueTypeCollection(ttc) ->
      let tr =
        ttc |> List.map (fun ttcelem ->
          Otfm.decoder_of_ttc_element ttcelem >>= fun dcdr ->
          Otfm.postscript_name dcdr >>= fun nameopt ->
          return (nameopt, dcdr)
        ) |> List.map (function
          | Error(_)               -> Element("<error>", Evaled([]))
          | Ok((None, dcdr))       -> Element("<none>", Evaled([]))
          | Ok((Some(name), dcdr)) -> Element(name, Evaled(table_list dcdr))
        )
      in
      return tr

  | Otfm.SingleDecoder(dcdr) ->
      return (table_list dcdr)
      

let arg_spec_list = []

let font_file_loaded_ref = ref false


let rec loop errcopt rowhl tr =
  Terminal.show_tree rowhl tr;
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
        if rowhl >= (List.length tr) - 1 then rowhl else rowhl + 1
      in
        loop None rowhlnew tr

  | 'p' ->
      let rowhlnew =
        if rowhl <= 0 then rowhl else rowhl - 1
      in
        loop None rowhlnew tr

  | 'e' ->
      begin
        enter_tree rowhl tr;
        loop None rowhl tr;
      end

  | 'q' -> ()

  | _ ->
        loop (Some(c)) rowhl tr


and enter_tree rowhl tr =
  match tr with
  | []           -> ()
  | head :: tail ->
      if rowhl <= 0 then
        match head with
        | Element(_, Evaled([]))  -> ()
        | Element(_, Evaled(trc)) -> loop None 0 trc
        | Element(_, ToEval(_))   -> ()
      else
        enter_tree (rowhl - 1) tail


let handle_input fontfile =
  font_file_loaded_ref := true;
  let res =
    tree_of_font_file fontfile >>= fun tr ->
    begin
      Terminal.initialize ();
      try
        begin
          loop None 0 tr;
          Terminal.terminate ();
          return ()
        end
      with
      | Assert_failure(_, _, _) ->
          begin
            Terminal.terminate ();
            err (`Message("internal error"))
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
