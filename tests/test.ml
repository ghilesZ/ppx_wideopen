(* open[@parse.int] Z <- uncomment this to get the biginteger version*)

(* this function may overflow using integers, but can not with
   Zarith's Z module *)
let nb_solutions a b c =
  let d = b * b - 4 * a * c in
  if d > 0 then print_string "two solutions\n"
  else if d = 0 then print_string "one solutions\n"
  else print_string "no solutions\n"

(*e.g. with the following arguments (on 64bits)*)
let _ = nb_solutions 1 2147483648 (-1)
