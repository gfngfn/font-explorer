
open Tree


let highlight_color_id = 1
  (* -- any integer >= 1 -- *)

let highlight_text_color = Curses.Color.white
let highlight_bg_color   = Curses.Color.red

let xmaxref = ref 0
let ymaxref = ref 0

let yshift = 1

let ybottom i = !ymaxref - i

let displayed_row_max () =  ybottom 5

let row_center () =
  displayed_row_max () / 2 + yshift
  

let initialize () =
  let win = Curses.initscr () in
  let (ymax, xmax) = Curses.getmaxyx win in
  begin
    xmaxref := xmax;
    ymaxref := ymax;
    assert (Curses.cbreak ());
    assert (Curses.noecho ());
    assert (Curses.start_color ());
    assert (Curses.init_pair highlight_color_id highlight_text_color highlight_bg_color);
  end


let terminate () =
  Curses.endwin ()


let print_status msg =
  assert (Curses.mvaddstr (ybottom 2) 2 msg)


let print_error msg =
  assert (Curses.mvaddstr (ybottom 1) 2 msg)


let show_highlighted_line row s =
  begin
    Curses.attron (Curses.A.color_pair highlight_color_id);
    assert (Curses.mvaddstr (row + yshift) 1 ("*" ^ s));
    Curses.attroff (Curses.A.color_pair highlight_color_id);
  end


let show_line row s =
  assert (Curses.mvaddstr (row + yshift) 2 s)


let show_tree rowhl lst =
  let numhidden =
    if rowhl < row_center () then
      0
    else
      rowhl - row_center ()
  in
  let rec aux row lst =
    match lst with
    | [] -> ()
    | (Element(s, _)) :: tail ->
        begin
          begin
            if (row < numhidden || (row - numhidden) + yshift > displayed_row_max ()) then () else
              (if row = rowhl then show_highlighted_line else show_line) (row - numhidden) s;
          end;
          aux (row + 1) tail
        end
  in
  begin
    Curses.erase ();
    let topstr =
      if numhidden = 0 then
        "-------------------"
      else
        "- - - -  ^  - - - -"
    in
    assert (Curses.mvaddstr 0 2 topstr);
    aux 0 lst;
    let bottomstr =
      if List.length lst - numhidden >= displayed_row_max () then
        "- - - -  V  - - - -"
      else
        "-------------------"
    in
    assert (Curses.mvaddstr (displayed_row_max ()) 2 bottomstr);
    assert (Curses.refresh ());
  end
