open[@parse.int] Z

let nb_solutions a b c =
  let d = b * b - 4 * a * c in
  if d > 0 then 2
  else if d = 0 then 1
  else 0
