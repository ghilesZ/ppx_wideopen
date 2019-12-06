module type Integers = sig
  (* representation for integer values *)
  type t

  (* operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t

  (* operators *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t

  (* printing *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit

  (* parsing *)
  val parse : string -> t
end

module type Decimals = sig
  (* representation for decimal values *)
  type t

  (* operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t

  (* operators *)
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val ( ~-. ) : t -> t

  (* int conversion *)
  val round_up   : t -> int
  val round_down : t -> int
  val of_int : int -> t

  (* printing *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit

  (* parsing *)
  val parse : string -> t
end

module type All = sig
  (* representation for both decimal and integer values *)
  type t

  (* operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val neg : t -> t

  (* operators *)
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t
  val ( ~-. ) : t -> t

  (* both integer and float operators can be used *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t

  (* printing *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit

  (* parsing *)
  val parse : string -> t
end

module Float = struct
  include Float

  let round_down x = x |> floor |> int_of_float
  let round_up x = x |> ceil |> int_of_float

  let ( +. ) = add
  let ( *. ) = mul
  let ( /. ) = div
  let ( -. ) = sub
  let ( ~-. ) = neg

  let print = Format.pp_print_float
  let parse = float_of_string
end

module Rationals = struct

  let round_down = Q.to_int
  let round_up x =
    let x' = round_down x in
    if (Q.of_int x') = x then x'
    else x' + 1

  include Q

  let ( +. ) = add
  let ( *. ) = mul
  let ( /. ) = div
  let ( -. ) = sub
  let ( ~-. ) = neg

  let print = Format.pp_print_float
  let parse = of_string
end
