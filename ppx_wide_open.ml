open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

(* Removes all occurences of the character '_' to make the parse
   function of the module bein used more resilient *)
let remove__ s =
  let nb_occ = ref 0 in
  String.iter (function '_' -> incr nb_occ | _ -> ()) s;
  let s' = Bytes.make (String.length s - !nb_occ) ' ' in
  let nb_cur = ref 0 in
  String.iteri (fun i c ->
      if c = '_' then incr nb_cur
      else Bytes.set s' (i- !nb_cur) c) s;
  Bytes.to_string s'

(* given an ast fragment representing a string 'c', builds the ast
   fragment for '(parse c)' *)
let parse_it c loc =
  let c = remove__ c in
  let id = Exp.ident (lid "parse") in
  Exp.apply ~loc:loc id [Nolabel,str c]

let replace const mode loc =
  match const,mode with
  | Pconst_integer(c,None),(`All | `Ints) -> parse_it c loc
  | Pconst_float(c,None),(`All | `Floats) -> parse_it c loc
  | c,_ -> Exp.constant ~loc c

(* replaces both float and integer litterals [lit] by [parse "lit"]*)
let parse_mapper_const _ mode =
  let handle mapper = function
    | {pexp_desc = Pexp_constant(x); pexp_loc; _ }-> replace x mode pexp_loc
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

(* when a [let open%replace.integers M in e] is met,
   rewrites [e] using parse_mapper *)
let expr_mapper mapper argv =
  let exprf default_expr mapper = function
    | {pexp_desc =
         Pexp_extension(
             ({txt="replace.int";_}),
             PStr([{pstr_desc=
                      Pstr_eval({pexp_desc=Pexp_open(op,exp);_} as pstr,
                                attr);
                    _}]))
      ; pexp_loc; _} ->
       let ofs = parse_mapper_const argv `Ints in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op,exp') in
       {pstr with pexp_desc = ope}
    | {pexp_desc =
         Pexp_extension(
             ({txt="replace.float";_}),
             PStr([{pstr_desc=
                      Pstr_eval({pexp_desc=Pexp_open(op,exp);_} as pstr,
                                attr);
                    _}]))
      ; pexp_loc; _} ->
       let ofs = parse_mapper_const argv `Floats in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op,exp') in
       {pstr with pexp_desc = ope}

    | {pexp_desc =
         Pexp_extension(
             ({txt="replace.all";_}),
             PStr([{pstr_desc=
                      Pstr_eval({pexp_desc=Pexp_open(op,exp);_} as pstr,
                                attr);
                    _}]))
      ; pexp_loc; _} ->
       let ofs = parse_mapper_const argv `All in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op,exp') in
       {pstr with pexp_desc = ope}

    |  x -> default_expr mapper x
  in
  {mapper with expr = exprf mapper.expr}

let () = register "wide" (expr_mapper default_mapper)
