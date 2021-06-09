(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2021 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

(*********************************************)
(*     Useful functions and data types       *)
(*********************************************)

(* Raise an error with a message m *)

let error m = Failure m |> raise

(* Extract a value from an option *)

let get_exn o = match o with
  | Some e -> e
  | _ -> error "Empty option!"

let is_min ls min = 
  List.for_all (fun e -> min <= e) ls

let print_offset _ = 
  Printf.printf "  "

(* Comparison function for increasing numbers *)

let inc_cmp x y = 
  if x > y then 1
  else if x < y then -1
  else 0

(* Printing a sub-array of elements *)

let print_int_sub_array l u arr =
  assert (l <= u);
  assert (u <= Array.length arr);
  Printf.printf "[| ";
  for i = l to u - 1 do
    Printf.printf "%d" arr.(i);
    if i < u - 1
    then Printf.printf "; "
    else ()      
  done;
  Printf.printf " |] "

(* Print the entire array *)

let print_int_array arr = 
  let len = Array.length arr in
  print_int_sub_array 0 len arr
