open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

(* Removes all occurences of the character '_'  *)
let remove__ s =
  let nb_occ = ref 0 in
  String.iter (function '_' -> incr nb_occ | _ -> ()) s;
  let s' = Bytes.make (String.length s - !nb_occ) ' ' in
  let nb_cur = ref 0 in
  String.iteri (fun i c ->
      if c = '_' then incr nb_cur
      else Bytes.set s' (i- !nb_cur) c) s;
  Bytes.to_string s'

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

(* replaces all float litterals [lit] by [parse "lit"]*)
let parse_mapper_float _ =
  let handle mapper = function
    | {pexp_desc = Pexp_constant(Pconst_float(c,None));
       pexp_loc; _ }->
       let id = Exp.ident (lid "parse") in
       Exp.apply ~loc:pexp_loc id [Nolabel,str c]
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

let with_constr module_expr module_name loc =
  let t =
    {ptype_name = Location.mkloc "t" loc;
     ptype_params = [];
     ptype_cstrs = [];
     ptype_kind = Ptype_abstract;
     ptype_private = Public;
     ptype_manifest =
       Some ({ptyp_desc=Ptyp_constr ((lid (module_name^".t")),[]);
              ptyp_loc=loc;
              ptyp_loc_stack=[];
              ptyp_attributes=[];
         });
     ptype_attributes=[];
     ptype_loc=loc;
    }
  in
  let constr = [Pwith_type ((lid "t"),t)] in
  {module_expr with pmty_desc = Pmty_with (module_expr,constr)}

(* The three module types available in Wideopen *)
let decimal_type loc = {
    pmty_desc = Pmty_ident (lid "Wideopen.Decimals");
    pmty_loc = loc;
    pmty_attributes = []
  }

let integer_type loc = {
    pmty_desc = Pmty_ident (lid "Wideopen.Integers");
    pmty_loc = loc;
    pmty_attributes = []
  }

let all_type loc = {
    pmty_desc = Pmty_ident (lid "Wideopen.All");
    pmty_loc = loc;
    pmty_attributes = []
  }

(* transforms an [open M] into [open M:mtype]*)
let constr opd mtype loc =
  let open_expr = opd.popen_expr in
  let ope = {open_expr with pmod_desc = Pmod_constraint(open_expr,mtype)} in
  {
  	popen_expr = ope;
  	popen_override = Asttypes.Fresh;
  	popen_loc = loc;
  	popen_attributes = [];
  }

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
       let it = integer_type pexp_loc in
       let op_constrained = constr op (with_constr it "Int" pexp_loc) pexp_loc in
       let ofs = parse_mapper_int argv in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op_constrained,exp') in
       {pstr with pexp_desc = ope}
    | {pexp_desc =
         Pexp_extension(
             ({txt="replace.float";_}),
             PStr([{pstr_desc=
                      Pstr_eval({pexp_desc=Pexp_open(op,exp);_} as pstr,
                                attr);
                    _}]))
      ; pexp_loc; _} ->
       let it = decimal_type pexp_loc in
       let op_constrained = constr op (with_constr it "Float" pexp_loc) pexp_loc in
       let ofs = parse_mapper_float argv in
       let exp' = ofs.expr ofs exp in
       let ope = Pexp_open(op_constrained,exp') in
       {pstr with pexp_desc = ope}

    |  x -> default_expr mapper x
  in
  {mapper with expr = exprf mapper.expr}

let () = register "wide" (expr_mapper default_mapper)
