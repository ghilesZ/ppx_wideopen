let split_itv (a,b) =
  let open%wide Z in
  let mid = (b - a) / 2 + a in
  (a,mid),(mid+1,b)

let double a =
  let open%wide Float in
  mul a 2
