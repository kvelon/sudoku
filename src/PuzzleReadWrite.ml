open Util
open ReadingFiles

type puzzle = int array array

let file_to_puzzle filename = 
  let str_lst = read_file_to_strings filename in
  let pz = Array.make_matrix 9 9 0 in
  List.iteri (fun r str ->
    let l = (String.split_on_char ' ' (String.trim str)) in
    List.iteri (fun c e -> 
      pz.(r).(c) <- int_of_string @@ String.trim e) l
  ) str_lst;
  pz
