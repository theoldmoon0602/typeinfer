open Syntax

exception Unimplemented

type typenv = (string * typ) list (* ("x", IntT); ("y", VarT("'y")); ... *)
type typsubst = (string * typ) list (* ("'y", IntT); ("'z", VarT("'y")); ... *)

let rec print_typenv typenv =
  match typenv with
  |(name, t)::xs -> begin
    print_string (name^"->"^(string_of_typ t)^",");
    print_typenv xs
  end
  |_ -> ()

let rec newtyvar name typenv =
  match List.assoc_opt name typenv with
  |Some(_) -> newtyvar ("'"^name) typenv
  |None -> VarT(name)

let rec remove_var name typenv =
  match typenv with
  |(name', t)::xs ->
      if name = name' then xs
      else (name',t)::(remove_var name typenv)
  |[] -> []


let rec occurs var_name typ =
  if var_name = typ then true
  else
    match typ with
    |FunT(argt, rett) -> (occurs var_name argt) || (occurs var_name rett)
    |_ -> false

(* replace t with ty *)
let rec replace_typ (t: typ) name (ty: typ): typ =
  match t with
  |VarT(name') -> if name = name' then ty else t
  |FunT(argt, rett) -> FunT(replace_typ argt name ty, replace_typ rett name ty)
  |_ -> t

let apply_substs (t: typ) (s: typsubst): typ =
  List.fold_right (fun (name, ty) t -> replace_typ t name ty) s t

let rec unify_one (t1: typ) (t2: typ): typsubst =
  match (t1, t2) with
  |(VarT(name1), VarT(name2)) ->
      if name1 = name2 then []
      else [(name2, t1)]
  |(VarT(name), _) ->
      if occurs t1 t2 then raise (TypeError "not unifiable") 
      else [(name, t2)]
  |(_, VarT(name)) ->
      if occurs t2 t1 then raise (TypeError "not unifiable")
      else [(name, t1)]
  |(FunT(argt1, rett1), FunT(argt2, rett2)) ->
      unify [(argt1, argt2); (rett1, rett2)]
  |(_, _) ->
      if t1 = t2 then []
      else raise (TypeError ("type mismatched:"^(string_of_typ t1)^", "^(string_of_typ t2)))

and unify typs =
  match typs with
  |(t1, t2)::xs ->
      let substs = unify xs in
      let subst = unify_one (apply_substs t1 substs) (apply_substs t2 substs) in
      subst @ substs (* list concatenation *)
  |[] -> []

let subst_to_typenv typenv subst =
  List.map (fun (name, t) -> (name, apply_substs t subst)) typenv 

let rec substitute tvar t typenv =
  match typenv with
  |(name, tvar')::xs ->
      if tvar = tvar' then
        (name, t)::(substitute tvar t xs)
      else
        (name, tvar')::(substitute tvar t xs)
  |[] -> []

let rec typeinfer exp typenv =
  match exp with
  |NumLit(_) -> (NumT, typenv)
  |BoolLit(_) -> (BoolT, typenv)
  |Var(s) -> begin
    match List.assoc_opt s typenv with
    |Some(t) -> (t, typenv)
    |None ->
        let tvar = newtyvar s typenv in
        let typenv = (s, tvar)::typenv in
        (tvar, typenv)
  end
  |Binop(op, lhe, rhe) -> begin
    let lht, typenv = typeinfer lhe typenv in
    let rht, typenv = typeinfer rhe typenv in
    match op with
      |Add |Mul ->
          let typenv = subst_to_typenv typenv (unify [(lht, NumT); (rht, NumT)]) in
          (NumT, typenv)
      |Lt |Gt ->
          let typenv = subst_to_typenv typenv (unify [(lht, NumT); (rht, NumT)]) in
          (BoolT, typenv)
      |And|Or ->
          let typenv = subst_to_typenv typenv (unify [(lht, BoolT); (rht, BoolT)]) in
          (BoolT, typenv)
  end
  |If(cond, t, e) -> begin
    let condt, typenv = typeinfer cond typenv in
    let typenv = subst_to_typenv typenv (unify [(condt, BoolT)]) in
    let tt, typenv = typeinfer t typenv in
    let et, typenv = typeinfer e typenv in
    let typenv = subst_to_typenv typenv (unify [(tt, et)]) in
    typeinfer t typenv
  end
  |Fun(arg, body) ->
    let _, typenv = typeinfer (Var(arg)) typenv in
    let bodyt, typenv = typeinfer body typenv in
    let argt, _ = typeinfer (Var(arg)) typenv in
    let typenv = remove_var arg typenv in
    (FunT(argt, bodyt), typenv)
  |App(fune, arge) ->
    let funt, typenv = typeinfer fune typenv in
    let argt, typenv = typeinfer arge typenv in
    let subst = unify [(argt, arg_typ funt)] in
    let typenv = subst_to_typenv typenv subst in
    (apply_substs (ret_typ funt) subst, typenv)


let repl () =
  let typenv = ref [] in
  while true do
    print_string ">";
    flush stdout;
    let program = Parser.main Lexer.main (Lexing.from_channel stdin) in
    print_endline (string_of_expr program);
    try
      let t, typenv' = typeinfer program [] in
      typenv := typenv';
      print_endline (string_of_typ t);
      print_string "["; 
      print_typenv typenv';
      print_endline "]"; 
    with
    TypeError(e) -> begin
      print_endline e;
    end
  done


let () =
  repl ()

