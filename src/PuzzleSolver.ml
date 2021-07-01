open Puzzle
open PuzzleReadWrite

let is_valid pz (row: int) (col: int) (n: int) = 

  let check_row () = 
    let valid = ref true in
    for c = 0 to 8 do
      valid := !valid && 
        (pz.(row).(c) <> n || c = col)
    done;
    !valid
  in

  let check_col () = 
    let valid = ref true in
    for r = 0 to 8 do
      valid := !valid &&
        (pz.(r).(col) <> n || r = row)
    done;
    !valid
  in

  let check_square () =
    let rs = (row / 3) * 3 in
    let cs = (col / 3) * 3 in
    let valid = ref true in
    for r = rs to rs + 2 do
      for c = cs to cs + 2 do
        valid := !valid &&
        (pz.(r).(c) <> n || (r, c) = (row, col))
      done
    done;
    !valid
  in  
  check_row () && 
  check_col () &&
  check_square ()
      
let rec solver pz r c = 

  let rec loop num = 
    if num = 10 then false
    else if is_valid pz r c num
    then begin
      pz.(r).(c) <- num;
      pp_puzzle pz;
      if solver pz r (c + 1)
      then true
      (* Back-tracking *)
      else (
        pz.(r).(c) <- 0;
        loop (num + 1)
      ) 
      end
    else loop (num + 1)
  in 

  if r > 8
  then true
  else if c > 8
  then solver pz (r + 1) 0
  else if pz.(r).(c) <> 0
  then solver pz r (c + 1)
  else loop 1

let solve_puzzle pz = 
  let b = solver pz 0 0 in
  if b
  then ()
  else print_endline "Puzzle is invalid"


(********************)
(*      Tests       *)
(********************)

let%test "Solver test 1" = 
  let p = file_to_puzzle "/home/kvelon/data-structures/sudoku/resources/basic_unsolved.txt" in
  solve_puzzle p;
  puzzle_complete p

let%test "Solver test 2" = 
  let p = file_to_puzzle "/home/kvelon/data-structures/sudoku/resources/medium_unsolved.txt" in
  solve_puzzle p;
  puzzle_complete p