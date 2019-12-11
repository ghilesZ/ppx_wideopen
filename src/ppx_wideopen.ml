open Migrate_parsetree.Ast_408
open Parsetree
open Ast_mapper
open Ast_helper

(* Removes all occurences of the character '_' to make the 'parse'
   function of the module being used more resilient *)
let remove__ s =
  let nb_occ = ref 0 in
  String.iter (function '_' -> incr nb_occ | _ -> ()) s;
  let s' = Bytes.make (String.length s - !nb_occ) ' ' in
  let nb_cur = ref 0 in
  String.iteri (fun i -> function
      | '_' -> incr nb_cur
      | c   -> Bytes.set s' (i- !nb_cur) c) s;
  Bytes.to_string s'

(* given an ast fragment representing a string 'c', builds the ast
   fragment for '(fname c)' *)
let parse c fname loc =
  let c = remove__ c in
  let id = Exp.ident (Location.mkloc (Longident.parse fname) loc) in
  Exp.apply ~loc:loc id [Nolabel,Exp.constant (Pconst_string (c,None))]

let replace const name fname loc =
  match const,name with
  | Pconst_integer(c,None),("parse.all" | "parse.int")
  | Pconst_float(c,None),("parse.all" | "parse.float") -> parse c fname loc
  | c,_ -> Exp.constant ~loc c

(* replaces both float and integer litterals [lit] by [parse "lit"]*)
let parse_mapper name fname =
  let handle mapper = function
    | {pexp_desc = Pexp_constant x; pexp_loc;_}-> replace x name fname pexp_loc
    |  x -> default_mapper.expr mapper x
  in
  {default_mapper with expr = handle}

(* can we handle the given attribute? *)
let check_attr a =
  match a.attr_name.txt with
  | "parse.int" | "parse.float" | "parse.all" -> true
  | _ -> false

(* given the payload of an attribute, computes the name of the
   function to be used for the parsing of litterals *)
let get_fname payload loc =
  match payload with
  | PStr [] -> "of_string"
  | PStr [{pstr_desc =
             Pstr_eval
               ({pexp_desc =
                   Pexp_apply
                     ({pexp_desc = Pexp_ident {txt = Lident "using";_};_},
                      [(Nolabel,{pexp_desc = Pexp_ident{txt=Lident fname;_};_})]);
                 _},_); _}] -> fname
  | _ -> Format.printf "%a\n%!" Location.print_loc loc;
         failwith "wrong payload for parse attribute. Should be \"using ident\""

(* when a [let open[parse.int] M in e] is met,
   rewrites [e] using parse_mapper *)
let open_wide_mapper =
  let handle_expr mapper expr =
    match expr.pexp_desc with
    | Pexp_open(op,exp) ->
       (match List.find check_attr expr.pexp_attributes with
        | attr ->
           let fname = get_fname attr.attr_payload attr.attr_loc in
           let ofs = parse_mapper attr.attr_name.txt fname in
           let exp' = ofs.expr ofs exp in
           let ope = Pexp_open(op,exp') in
           {expr with pexp_desc = ope;
                      (* we keep the others potential attributes *
                       * to not interfere with other PPXs *)
                      pexp_attributes = List.filter ((<>) attr) expr.pexp_attributes}
        | exception Not_found -> expr)
    |  _ -> default_mapper.expr mapper expr
  in
  let handle_str mapper =
    (* if one str_it is an open[@parse...], then the next str_it will
       be mapped using handle_expr. Otherwise it keeps using the same mapper *)
    let rec aux (res,map) = function
      | [] -> List.rev res
      | ({pstr_desc=Pstr_open opd;_} as h)::tl ->
         (match List.find check_attr opd.popen_attributes with
          | attr ->
             let fname = get_fname attr.attr_payload attr.attr_loc in
             let rmv_attrs = List.filter ((<>) attr) opd.popen_attributes in
             let opd' = {opd with popen_attributes = rmv_attrs} in
             let h' = {h with pstr_desc = Pstr_open opd'} in
             let map' = parse_mapper attr.attr_name.txt fname in
             aux (h'::res,map') tl
          | exception Not_found -> aux (h::res,map) tl
         )
      | h::tl -> let h' = map.structure_item map h in
                 aux (h'::res,map) tl
    in
    aux ([],mapper)
  in
  {default_mapper with expr = handle_expr; structure = handle_str}

let () =
  let open Migrate_parsetree in
  Driver.register ~name:"ppx_openwide" ~args:[]
    Versions.ocaml_408 (fun _config _cookies -> open_wide_mapper)
