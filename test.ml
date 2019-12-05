let mid a b =
  let open (Float:Replace.Decimals) in
  a +. (b -. a) /. (parse "2")
