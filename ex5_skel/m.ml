type program = exp
and exp = 
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

exception TypeError

type typ = TyInt | TyBool 
	| TyFun of typ * typ (* t1 -> t2 *) 
	| TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list (* t1 = t2 *)

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the substitution and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
= fun tenv e ty -> 
  match e with
  | CONST n ->[(ty,TyInt)]
  
  | TRUE -> [(ty,TyBool)]
  
  | FALSE -> [(ty,TyBool)]
  
  | VAR x -> let varType = TEnv.find tenv x in [(ty,varType)]
  
  | ADD (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  
  | SUB (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  
  | MUL (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  
  | DIV (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  
  | ISZERO e -> [(ty,TyBool)]@(gen_equations tenv e TyInt)
  
  | READ -> [(ty,TyInt)]
  
  | IF (e1,e2,e3) ->  (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  
  | LET (x,e1,e2)-> 
  let xType = fresh_tyvar () in
  let newEnv = TEnv.extend (x,xType) tenv in
  (gen_equations tenv e1 xType) @ (gen_equations newEnv e2 ty) 
  
  | LETREC (f,x,e1,e2) ->
  let xType = fresh_tyvar() in
  let funType = fresh_tyvar() in
  let newEnv = TEnv.extend (f,TyFun(xType,funType)) tenv in
  let newEnv' = TEnv.extend (x,xType) newEnv in
  (gen_equations newEnv' e1 funType)@(gen_equations newEnv e2 ty)


  
  | PROC (x,e) -> 
  let xType = fresh_tyvar() in
  let procType = fresh_tyvar() in
  let newEnv = (TEnv.extend (x,xType) tenv) in
  [(ty,TyFun(xType,procType))]@(gen_equations newEnv e procType)

  | CALL (e1,e2) -> let callType = fresh_tyvar() in
  (gen_equations tenv e1 (TyFun(callType,ty)))@(gen_equations tenv e2 callType)



let rec unify : typ -> typ -> Subst.t -> Subst.t
= fun t1 t2 subst ->
match (t1,t2) with
 |(TyInt,TyInt) -> subst
 |(TyBool,TyBool) -> subst
 |(TyVar x1,TyVar x2) -> Subst.extend x1 t2 subst
 |(TyVar x,TyFun(tf1,tf2)) -> let subst' = unify tf1 tf2 [] in
 if(List.mem_assoc x subst') then raise TypeError else Subst.extend x t2 subst
 |(TyVar x,_) -> Subst.extend x t2 subst
 |(_,TyVar x) -> unify t2 t1 subst 
 |(TyFun (tf1,tf2),TyFun(tf1',tf2')) ->
    let subst' = unify tf1 tf1' subst in
    let subst'' = unify (Subst.apply tf2 subst') (Subst.apply tf2' subst') subst' in
    subst''
 |(_,_) -> raise TypeError



let rec unifyall : typ_eqn -> Subst.t -> Subst.t
= fun eqns subst ->
match eqns with
|[] -> subst
|(t1,t2)::tl -> let t1' = Subst.apply t1 subst in let t2' = Subst.apply t2 subst in 
let subst' = unify t1' t2' subst in unifyall tl subst'

let solve : typ_eqn -> Subst.t
= fun eqns ->
unifyall eqns []
  






(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)