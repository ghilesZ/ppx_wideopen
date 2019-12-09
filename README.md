# Why Wideopen?
The shadowing trick allows the users to overwrite the usual arithmetic
operators ````(+, -, ...)```` with their own by simply doing ````open
My_module````. However the litterals in the program are still
interpreted as integer or floats by OCaml which forces the programmer
to write boilerplate code to handle these cases.

Open-wide is a syntax-extension that allows you to switch easily the
numeric representation used for integers and floats without having to
care about the parsing of litterals. It allows you to use a custom
parsing utility for OCaml's litterals using by default the `of_string`
function of the specified module.

For example, the following piece of code which uses the Zarith
liibrary computes the number of solutions of a quadratic equation:

````OCaml
  let nb_solutions a b c =
    let open[@replace.int] Z in
    let delta = b * b - 4 * a * c in
    if delta > 0 then 2
    else if delta = 0 then 1
    else 0
````

This is syntactic sugar for:

````OCaml
  let nb_solutions a b c =
    let open Z in
    let delta = (b * b) - (of_string "4") * a * c in
    if delta > (of_string "0")
    then of_string "2"
    else if delta = (of_string "0") then of_string "1" else of_string "0"
````

## How it works?
Whenever an ````[@replace...]```` annotation is met, the corresponding
litterals are replaced with ````ofstring
"str_representing_the_litteral" ````. Also, as it works on the Parsetree of
OCaml, we are able to handle litterals of arbitrary size
(e.g. greater than 2^64), given that the parsing function being
used accepts it. 


## The different annotations:
Wideopen provides three different annotation:

- ````[@replace.int]```` which replaces the integer litterals (the ````Pconst_integers```` constructor of the ````Parsetree````)
  
- ````[@replace.float]```` which replaces the float litterals (the ````Pconst_float```` constructor of the ````Parsetree````)

- ````[@replace.all]```` which replaces both float and integer litterals.

Note that the latter annotation allows you in particular to manipulate
"integers" and "floats" indiferently as a "bigger type" (Zarith's
rationals using the ````Q```` module for example) without having to care about
int-float conversion and operators.

## Customizing the parsing utility:
By default, Wideopen uses the ````of_string```` of the specified
module. To avoid having to define such a function within our modules,
we can specify the name of the function to be used for the parsing 
using the ````with```` construction as in the following examples:

````OCaml
let open[@replace.all using parse] Mymodule in ...
````
