open[@parse.int] Z

let nb_solutions a b c =
  let delta = b * b - 4 * a * c in
  if delta > 0 then 2
  else if delta = 0 then 1
  else 0

let _ =
  Format.printf "%a\n" pp_print  (nb_solutions 2 9999999999999999999999999999 4)
