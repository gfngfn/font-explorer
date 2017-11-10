
open Tree


let highlight_color_id = 1
  (* -- any integer >= 1 -- *)

let highlight_text_color = Curses.Color.white
let highlight_bg_color   = Curses.Color.red

let xmaxref = ref 0
let ymaxref = ref 0


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
  assert (Curses.mvaddstr (!ymaxref - 2) 2 msg)


let print_error msg =
  assert (Curses.mvaddstr (!ymaxref - 1) 2 msg)


let show_highlighted_line row s =
  begin
    Curses.attron (Curses.A.color_pair highlight_color_id);
    assert (Curses.mvaddstr row 1 ("*" ^ s));
    Curses.attroff (Curses.A.color_pair highlight_color_id);
  end


let show_line row s =
  assert (Curses.mvaddstr row 2 s)


let show_tree rowhl lst =
  let rec aux row lst =
    match lst with
    | [] -> ()
    | (Element(s, _)) :: tail ->
        begin
          (if row = rowhl then show_highlighted_line else show_line) row s;
          aux (row + 1) tail
        end
  in
  begin
    Curses.erase ();
    show_line 0 "---- ---- ---- ----";
    aux 1 lst;
    assert (Curses.refresh ());
  end
