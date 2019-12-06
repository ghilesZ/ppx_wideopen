open Wideopen

let mid a b =
  let open%replace.float Rationals in
  a +. (b -. a) /. 2.
