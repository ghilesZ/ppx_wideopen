module type Integers = sig
  (* representation for integer values *)
  type t

  (* operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  val pow : t -> t -> t

  (* operators *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

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

  val pow : t -> int -> t

  (* operators *)
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

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

  val pow : t -> t -> t

  (* operators *)
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

  (* both integer and float operators can be used *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t

  (* printing *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit

  (* parsing *)
  val parse : string -> t
end
