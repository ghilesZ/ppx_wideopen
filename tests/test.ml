open[@parse.int] Z

let nb_solutions a b c =
  let d = b * b - 4 * a * c in
  if d > 0 then let open[@parse.int] Q in 2
  else if d = 0 then  Q.(1)[@parse.int]
  else Q.(0)[@parse.int]
