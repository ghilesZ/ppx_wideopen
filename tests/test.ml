module Rat = struct
  let rat_of_string = Q.of_string
end

open[@parse.int] Z

let z=2

let nb_solutions a b c =
  let open[@parse.float using rat_of_string] Rat in
  let d = b * b - 4 * a * c in
  if d > 0 then 2.
  else if d = 0 then 1.
  else 0.
