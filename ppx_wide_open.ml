open Parsetree
open Ast_mapper
open Ast_helper
open Ast_convenience

(* Removes all occurences of the character '_' to make the 'parse'
   function of the module being used more resilient *)
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
let parse_mapper_const mode =
  let handle mapper = function
    | {pexp_desc = Pexp_constant(x); pexp_loc; _ }-> replace x mode pexp_loc
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

(* search for a given attribute *)
let find_attribute attrs name =
  let rec aux ((contains, rest) as acc) = function
    | [] -> acc
    | h::tl -> if h.attr_name.txt = name then true,(List.rev rest) @ tl
               else aux (false,h::rest) tl
  in
  aux (false,[]) attrs

(* when a [let open%replace.int M in e] is met,
   rewrites [e] using parse_mapper *)
let replace_mapper _ =
  let handle_expr mapper expr =
    match expr.pexp_desc with
    | Pexp_open(op,exp) ->
       (try
          let mode,rest =
            let res,rest = find_attribute expr.pexp_attributes "replace.int" in
            if res then `Ints,rest
            else
              let res,rest = find_attribute expr.pexp_attributes "replace.float" in
              if res then `Floats,rest
              else
                let res,rest = find_attribute expr.pexp_attributes "replace.all" in
                if res then `All,rest
                else raise Exit
          in
          let ofs = parse_mapper_const mode in
          let exp' = ofs.expr ofs exp in
          let ope = Pexp_open(op,exp') in
          {expr with pexp_desc = ope; pexp_attributes = rest}
        with Exit -> expr)
    |  _ -> default_mapper.expr mapper expr
  in
  {default_mapper with expr = handle_expr}

let () = register "wide" replace_mapper
