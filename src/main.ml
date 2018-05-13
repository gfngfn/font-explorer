
open Tree


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



let leaf s =
  Element(s, Evaled([]))


let head_content dcdr () =
  Otfm.head dcdr >>= fun h ->
  return [
    leaf (Printf.sprintf "flags       : %04X" h.Otfm.head_flags);
    leaf (Printf.sprintf "fontRevision: %d" (Int32.to_int h.Otfm.head_font_revision));
    leaf (Printf.sprintf "unitsPerEm  : %d" h.Otfm.head_units_per_em);
    leaf (Printf.sprintf "created     : %s" (Int64.to_string (Int64.of_float h.Otfm.head_created)));
    leaf (Printf.sprintf "modified    : %s" (Int64.to_string (Int64.of_float h.Otfm.head_modified)));
    leaf (Printf.sprintf "xMin        : %d" h.Otfm.head_xmin);
    leaf (Printf.sprintf "yMin        : %d" h.Otfm.head_ymin);
    leaf (Printf.sprintf "xMax        : %d" h.Otfm.head_xmax);
    leaf (Printf.sprintf "yMax        : %d" h.Otfm.head_ymax);
  ]


let table_list dcdr =
  Otfm.table_list dcdr >>= fun taglst ->
  let tagstrlst = taglst |> List.map Otfm.Tag.to_bytes in
  let tagtrlst =
    tagstrlst |> List.map (fun tagstr ->
      let children =
        match tagstr with
        | "head" -> ToEval(head_content dcdr)
        | _      -> Evaled([])
      in
      Element(tagstr, children)
    )
  in
  return tagtrlst


let tree_of_font_file fontfile =
  string_of_file fontfile >>= fun s ->
  Otfm.decoder (`String(s)) >>= function
  | Otfm.TrueTypeCollection(ttc) ->
      begin
        try
          let tr =
            ttc |> List.map (fun ttcelem ->
              Otfm.decoder_of_ttc_element ttcelem >>= fun dcdr ->
              Otfm.postscript_name dcdr >>= fun nameopt ->
              table_list dcdr >>= fun lst ->
              return (nameopt, lst)
            ) |> List.map (function
              | Error(e)              -> raise (ErrorJump(e))
              | Ok((None, lst))       -> Element("<no-postscript-name>", Evaled(lst))
              | Ok((Some(name), lst)) -> Element(name, Evaled(lst))
            )
          in
          return tr
        with
        | ErrorJump(e) -> err e
      end

  | Otfm.SingleDecoder(dcdr) ->
      table_list dcdr >>= fun lst ->
      return lst


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
      enter_tree rowhl tr >>= fun () ->
      loop None rowhl tr

  | 'q' -> return ()

  | _ ->
        loop (Some(c)) rowhl tr


and enter_tree rowhl tr =
  match tr with
  | [] ->
      return ()

  | head :: tail ->
      if rowhl <= 0 then
        match head with
        | Element(_, Evaled([])) ->
            return ()

        | Element(_, Evaled(trc)) ->
            loop None 0 trc

        | Element(_, ToEval(f)) ->
            f () >>= fun trc ->
            loop None 0 trc
      else
        enter_tree (rowhl - 1) tail


let main fontfile =
  let res =
    tree_of_font_file fontfile >>= fun tr ->
    begin
      Terminal.initialize ();
      try
        loop None 0 tr >>= fun () ->
        Terminal.terminate ();
        return ()
      with
      | Assert_failure(_, _, _) ->
          begin
            Terminal.terminate ();
            err (`Message("internal error"))
          end
    end
  in
  report_result res


let arg_spec_list = []

let srcinref = ref None

let specify_input src =
  srcinref := Some(src)


let () =
  Arg.parse arg_spec_list specify_input "";
  match !srcinref with
  | None        -> prerr_endline "No font file specified."; exit 1
  | Some(srcin) -> main srcin
