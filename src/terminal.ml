
let flush () =
  begin
    print_string "\x1b[2J";
    print_string "\x1b[0;0H";
  end


let show_highlighted_line row s =
  begin
    Printf.printf "\x1b[%d;0H\x1b[7m%s\x1b[0m" row s;
  end


let show_line row s =
  begin
    Printf.printf "\x1b[%d;0H%s" row s;
  end


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
    flush ();
    aux 1 lst;
  end
