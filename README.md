# Open-Wide
`let open%wide M in` is a syntax extension that allows you to parse OCaml's float and integer litterals with the `of_string` function of the specified module.

For example, the following:
````OCaml
  let split_itv (a,b) =
    let open%wide Z in
    let mid = (b - a) / 2 + a in
    (a,mid),(mid+1,b)
````

is syntactic sugar for:

````OCaml
  let split_itv (a,b) =
    let open Z in
    let mid = (b - a) / (of_string "2") + a in
    (a,mid),(mid+(of_string "1"),b)
````

As it works on the Parsetree of OCaml, it makes it possible to handle litterals of arbitrary size (e.g. greater than 2^64), given that the of_string function being used accepts it.  In particular, using Zarith, it allows you to manipulate "integers" and "floats" indiferently as rationals


````OCaml
  let foo bar = 
    let open%wide Q in
    2.5 * foo / (bar+1)
````
