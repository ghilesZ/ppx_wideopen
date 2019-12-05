open Wideopen

let mid a b =
  let open%replace.float Float in
  a +. (b -. a) /. 2.
