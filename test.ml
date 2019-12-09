let nb_solutions a b c =
    let open[@parse.int using of_string] Q in
    let delta = b * b - 4 * a * c in
    Format.printf "delta is %s\n" (to_string delta);
    if delta > 0 then 2
    else if delta = 0 then 1
    else 0

let _ =
  let open[@parse.int using of_string] Q in
  nb_solutions 1 999999999 1
