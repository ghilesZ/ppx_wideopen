# Why Open-Wide?
The shadowing trick allows the users to overwrite the usual arithmetic
operators ````(+, -, ...)```` with their own by simply doing ````open
My_module```` but the litterals in the program are however interpreted
as integer or floats by OCaml.

Open-wide is a library that allows you to switch easily the numeric
representation used for integers and floats including the parsing of
litterals. It provides a syntax extension that allows you to use a
custom parsing utility for OCaml's litterals with the `parse` function
of the specified module.

For example, the following piece of code which computes the number of
solutions of a quadratic equation:

````OCaml
  let nb_solutions a b c =
    let open%replace.int Wideopen.Zarith_Z in
    let delta = b * b - 4 * a * c in
    if delta > 0 then 2
    else if delta = 0 then 1
    else 0
````

is syntactic sugar for:

````OCaml
  let nb_solutions a b c =
    let open Wideopen.Zarith_Z in
    let delta = (b * b) - (((parse "4") * a) * c) in
    if delta > (parse "0")
    then parse "2"
    else if delta = (parse "0") then parse "1" else parse "0"
````

Note that te latter version wont be vulnerable to overflows as it uses
overflows.

## How it works?
Also, as it works on the Parsetree of OCaml, it makes it possible to handle
litterals of arbitrary size (e.g. greater than 2^64), given that the
````parse```` function being used accepts it.  In particular, using
Zarith, it allows you to manipulate "integers" and "floats"
indiferently as rationals using the ````replace.all```` extension.


````OCaml
  let foo bar = 
    let open%replace.all Rationals in
    2.5 * foo /. (bar+1)
````

Note that wideopen provides wrappers that define several modules with
handy operators and function name in a unified way for an easier
utilisation.
