let nb_solutions a b c =
  let open[@replace.int] Wideopen.Zarith_z in
  let delta = b * b - 4 * a * c in
  if delta > 0 then 2
  else if delta = 0 then 1
  else 0
