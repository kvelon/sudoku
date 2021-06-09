open Util

type puzzle = int array array

(***********************)
(*     Print puzzle    *)
(***********************)

let pp_puzzle (p : puzzle) =
  print_endline " _______________________ ";
  for r = 0 to 8 do
    Printf.printf "| %d %d %d " p.(r).(0) p.(r).(1) p.(r).(2);
    Printf.printf "| %d %d %d " p.(r).(3) p.(r).(4) p.(r).(5);
    Printf.printf "| %d %d %d |\n" p.(r).(6) p.(r).(7) p.(r).(8);
    if (r + 1) mod 3 = 0
    then print_endline " _______________________ ";
  done

(***********************)
(*       Checkers      *)
(***********************)

let square_coords = [|(0, 0); (0, 3); (0, 6);
                      (3, 0); (3, 3); (3, 6);
                      (6, 0); (6, 3); (6, 3)|]

let row_complete (p : puzzle) (r : int) =
  let l = Array.to_list p.(r) |>
          List.sort inc_cmp
  in
  l = [1;2;3;4;5;6;7;8;9]

let col_complete (p : puzzle) (c : int) = 
  let l = ref [] in
  for r = 0 to 8 do
    l := p.(r).(c) :: !l
  done;
  List.sort inc_cmp !l = [1;2;3;4;5;6;7;8;9]

let square_complete (p : puzzle) (r : int) (c : int) = 
  let rs = (r / 3) * 3 in
  let cs = (c / 3) * 3 in
  let l = ref [] in
  for row = rs to rs + 2 do
    for col = cs to cs + 2 do
      l := p.(row).(col) :: !l
    done
  done;
  List.sort inc_cmp !l = [1;2;3;4;5;6;7;8;9]

let puzzle_complete (p : puzzle) = 
  let complete = ref true 
  in
  (* Check rows, columns and squares *)
  let i = ref 0 in
  while !complete && !i < 9 do
    let (r, c) = square_coords.(!i) in
    complete := !complete && row_complete p !i && 
                col_complete p !i &&
                square_complete p r c;
    i := !i + 1
  done; 
  print_int !i;
  !complete  

(***********************)
(*      Examples       *)    
(***********************)

let p1 = [|
          [|1;1;1;1;1;1;1;1;1|];
          [|2;2;2;2;2;2;2;2;2|];
          [|3;3;3;3;3;3;3;3;3|];
          [|4;4;4;4;4;4;4;4;4|];
          [|5;5;5;5;5;5;5;5;5|];
          [|6;6;6;6;6;6;6;6;6|];
          [|7;7;7;7;7;7;7;7;7|];
          [|8;8;8;8;8;8;8;8;8|];
          [|9;9;9;9;9;9;9;9;9|];
         |]

let p2 = [|
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
          [|1;2;3;4;5;6;7;8;9|];
         |]

let p3 = [|
          [|3;4;5;8;7;1;9;2;6|];
          [|7;2;6;3;4;9;8;5;1|];
          [|8;9;1;2;5;6;4;7;3|];
          [|4;7;9;1;3;2;6;8;5|];
          [|1;6;2;5;9;8;7;3;4|];
          [|5;3;8;7;6;4;2;1;9|];
          [|9;1;3;4;2;7;5;6;8|];
          [|6;8;7;9;1;5;3;4;2|];
          [|2;5;4;6;8;3;1;9;7|];
         |]

(********************)
(*      Tests       *)
(********************)

let%test "Row check test 1" = 
  for i = 0 to 8 do
    assert (row_complete p2 i)
  done;
  true

let%test "Row check test 2" = 
  for i = 0 to 8 do
    assert (not @@ row_complete p1 i)
  done;
  true
  
let%test "Col check test 1" = 
  for i = 0 to 8 do
    assert (col_complete p1 i)
  done;
  true

let%test "Col check test 2" = 
  for i = 0 to 8 do
    assert (not @@ col_complete p2 i)
  done;
  true

let%test "Square check test" = 
  Array.iter (fun (r, c) -> 
    assert (square_complete p3 r c)) square_coords;
  true

let%test "Complete puzzle test" =
  puzzle_complete p3