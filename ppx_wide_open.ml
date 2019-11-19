open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

(* replaces all integer and float litterals [lit] by [of_string "lit"]*)
let of_string_mapper _ =
  let handle mapper = function
    | {pexp_desc = Pexp_constant(Pconst_float(c,None)|Pconst_integer(c,None));
       pexp_loc; _ }->
       let id = Exp.ident (lid "of_string")in
       Exp.apply ~loc:pexp_loc id [Nolabel,str c]
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

(* when a [let open%wide M in e] is met, rewrites [e] using of_string_mapper *)
let expr_mapper mapper argv =
  let exprf default_expr mapper = function
    | {pexp_desc =
         Pexp_extension(
             ({txt="wide";_}),
             PStr([{pstr_desc=
                      Pstr_eval({pexp_desc=Pexp_open(op,exp);_} as pstr,
                                attr);
                    _}]))
      ;_} ->
       let ofs = of_string_mapper argv in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op,exp') in
       {pstr with pexp_desc = ope}
    |  x -> default_expr mapper x
  in
  {mapper with expr = exprf mapper.expr}

let () = register "wide" (expr_mapper default_mapper)
