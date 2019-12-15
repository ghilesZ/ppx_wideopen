open Migrate_parsetree.Ast_408
open Parsetree
open Ast_mapper
open Ast_helper
open Location

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

(* fresh_id generator *)
let fresh_module_name =
  let cpt = ref 0 in
  let name = "PPX_wideopen_alias_" in
  fun () -> incr cpt; name^(string_of_int !cpt)

(* Builds the ast fragment corresponding to a module alias *)
let alias_pstr mod_name mod_expr loc =
  Str.mk ~loc (Pstr_module (Mod.mk ~loc mod_expr |> Mb.mk ~loc mod_name))

(* Builds the ast fragment corresponding to a letmodule alias *)
let alias_expr mod_name mod_expr loc body =
 Exp.letmodule ~loc mod_name (Mod.mk ~loc mod_expr) body

(* given a module name [Mn] and a function name [fn], builds the identifier
   [Mn.fn] *)
let id modname fname loc=
  {pexp_desc = Pexp_ident {txt = Ldot (Lident modname, fname); loc};
   pexp_loc_stack = [];
   pexp_loc = loc;
   pexp_attributes = [];}

(* given an ast fragment representing a string 'c', builds the ast
   fragment for '(fname c)' *)
let parse c modname fname loc =
  let c = remove__ c in
  let name = id modname fname loc in
  Exp.apply ~loc:loc name [Nolabel,Exp.constant (Pconst_string (c,None))]

(* replaces litterals [lit] by [fname "lit"] *)
let parse_mapper mode modulename fname =
  let replace const loc =
    match const,mode with
    | Pconst_integer(c,None),("parse.all" | "parse.int")
      | Pconst_float(c,None),("parse.all" | "parse.float") -> parse c modulename fname loc
    | c,_ -> Exp.constant ~loc c
  in
  let handle mapper = function
    | {pexp_desc = Pexp_constant x; pexp_loc;_}-> replace x pexp_loc
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
  | _ -> Format.printf "%a\n%!" print_loc loc;
         failwith "wrong payload for parse attribute. Should be \"using ident\""

(* get the name of the parsing utility using the payload, and removes
   from the attributes list the one that was used.  We keep the others
   potential attributes to not interfere with other PPXs.  Raises
   Not_found if no [@parse] attribute was found. *)
let deal_with_attr module_name attrs =
  let attr = List.find check_attr attrs in
  let fname = get_fname attr.attr_payload attr.attr_loc in
  let ofs = parse_mapper attr.attr_name.txt module_name fname in
  ofs,List.filter ((<>) attr) attrs

(* when a [let open[parse.int] M in e] is met,
   rewrites [e] using parse_mapper *)
let open_wide_mapper =
  let handle_expr mapper expr =
    match expr.pexp_desc with
    | Pexp_open(op,exp) ->
       let loc = expr.pexp_loc in
       (try
          let m_a = fresh_module_name() in
          let mapper,attrs = deal_with_attr m_a expr.pexp_attributes in
          let exp' = mapper.expr mapper exp in
          let op' = {op with popen_expr = Mod.mk ~loc:loc (Pmod_ident (mkloc (Longident.parse m_a) loc))} in
          let ope = Pexp_open(op',exp') in
          let mod_alias = alias_expr (mkloc m_a loc) op.popen_expr.pmod_desc loc (Exp.mk ~loc ope) in
          {mod_alias with pexp_attributes = attrs}
        with Not_found -> expr)
    |  _ -> default_mapper.expr mapper expr
  in
  let handle_str mapper =
    (* if one str_it is an open[@parse...], then the next str_it will
       be mapped using parse_mapper. Otherwise it keeps using the same mapper *)
    let rec aux (res,map) = function
      | [] -> List.rev res
      | ({pstr_desc=Pstr_open opd;_} as h)::tl ->
         let loc = h.pstr_loc in
         (try
            let m_a = fresh_module_name() in
            let map', attrs = deal_with_attr m_a opd.popen_attributes in
            let mod_alias = alias_pstr (mkloc m_a loc) opd.popen_expr.pmod_desc loc in
            let opd' = {opd with popen_attributes = attrs;
                                 popen_expr = Mod.mk ~loc:loc (Pmod_ident (mkloc (Longident.parse m_a) loc));}
            in
            let h' = {h with pstr_desc = Pstr_open opd'} in
            aux (h'::mod_alias::res,map') tl
          with Not_found -> aux (h::res,map) tl)
      | h::tl ->
         let h' = map.structure_item {map with expr = handle_expr} h
               |> map.structure_item map
         in
         aux (h'::res,map) tl
    in
    aux ([],mapper)
  in
  {default_mapper with expr = handle_expr; structure = handle_str}

let () =
  let open Migrate_parsetree in
  Driver.register ~name:"ppx_openwide" ~args:[]
    Versions.ocaml_408 (fun _config _cookies -> open_wide_mapper)
