open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

(* utility to build a single non recursive binding *)
let let_1 name value body loc =
  let pat = Pat.mk (Ppat_var (Location.mkloc name loc)) in
  let value = Exp.ident (lid name) in
  let binds = {
  	  pvb_pat = pat;
  	  pvb_expr = value;
  	  pvb_attributes = [];
  	  pvb_loc = loc
    }
  in
  Exp.mk (Pexp_let (Nonrecursive, [binds],body))

(* replaces all integer litterals [lit] by [of_string "lit"]*)
let parse_mapper_int _ =
  let handle mapper = function
    | {pexp_desc = Pexp_constant(Pconst_integer(c,None));
       pexp_loc; _ }->
       let id = Exp.ident (lid "parse") in
       Exp.apply ~loc:pexp_loc id [Nolabel,str c]
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

(* replaces all float litterals [lit] by [of_string "lit"]*)
let parse_mapper_float _ =
  let handle mapper = function
    | {pexp_desc = Pexp_constant(Pconst_float(c,None));
       pexp_loc; _ }->
       let id = Exp.ident (lid "parse") in
       Exp.apply ~loc:pexp_loc id [Nolabel,str c]
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

(* when a [let open%replace.integers M in e] is met,
   rewrites [e] using parse_mapper *)
let expr_mapper mapper argv =
  let exprf default_expr mapper = function
    | {pexp_desc =
         Pexp_extension(
             ({txt="replace.integers";_}),
             PStr([{pstr_desc=
                      Pstr_eval({pexp_desc=Pexp_open(op,exp);_} as pstr,
                                attr);
                    _}]))
      ;_} ->
       let ofs = parse_mapper_int argv in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op,exp') in
       {pstr with pexp_desc = ope}
    |  x -> default_expr mapper x
  in
  {mapper with expr = exprf mapper.expr}

let () = register "wide" (expr_mapper default_mapper)
