module type Integers = sig
  (* representation for integer values *)
  type t

  (* operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

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

  (* operators *)
  val ( +. ) : t -> t -> t
  val ( -. ) : t -> t -> t
  val ( *. ) : t -> t -> t
  val ( /. ) : t -> t -> t

  (* printing *)
  val to_string : t -> string
  val print : Format.formatter -> t -> unit

  (* parsing *)
  val parse : string -> t
end
