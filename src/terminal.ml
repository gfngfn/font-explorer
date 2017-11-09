

let highlight_color = 1


let initialize () =
  let _ = Curses.initscr () in
  begin
    assert (Curses.cbreak ());
    assert (Curses.noecho ());
    assert (Curses.start_color ());
    assert (Curses.init_pair highlight_color Curses.Color.white Curses.Color.red);
  end


let terminate () =
  Curses.endwin ()


let show_highlighted_line row s =
  begin
    Curses.attron (Curses.A.color_pair highlight_color);
    assert (Curses.mvaddstr row 1 ("*" ^ s));
    Curses.attroff (Curses.A.color_pair highlight_color);
  end


let show_line row s =
  assert (Curses.mvaddstr row 2 s)


let show_list rowhl lst =
  let rec aux row lst =
    match lst with
    | [] -> ()
    | s :: tail ->
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
