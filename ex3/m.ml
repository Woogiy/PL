type program = exp
and exp = 
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
| NEWREF of exp 
| DEREF of exp
| SETREF of exp * exp
| SEQ of exp * exp
| BEGIN of exp
and var = string

type value = 
Int of int 
| Bool of bool 
| Procedure of var * exp * env 
| RecProcedure of var * var * exp * env
| Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f
  
  (* environment *)
  let empty_env = []
  let extend_env (x,v) e = (x,v)::e
  let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

  

  (* memory *)
  let empty_mem = [] 
  let extend_mem (l,v) m = (l,v)::m
  let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

  (* use the function 'new_location' to generate a fresh memory location *)
  let counter = ref 0
  let new_location () = counter:=!counter+1;!counter

  exception NotImplemented
  exception UndefinedSemantics

  (*****************************************************************)
  (* TODO: Implement the eval function. Modify this function only. *)
  (*****************************************************************)

  let rec eval : exp -> env -> mem -> value * mem
  = fun exp env mem ->
  match exp with
  |CONST n -> (Int n, mem)


  |VAR x -> let l = (apply_env env x) in
  let v = (apply_mem mem (val2int l)) in
  (v,mem)


  |ADD (e1,e2) -> let (v1,mem1) = eval e1 env mem in
  let (v2,mem2) = eval e2 env mem1 in
  (Int((val2int v1) + (val2int v2)),mem2)


  |SUB (e1,e2) -> let (v1,mem1) = eval e1 env mem in
  let (v2,mem2) = eval e2 env mem1 in
  (Int((val2int v1) - (val2int v2)),mem2)


  |MUL (e1,e2) -> let (v1,mem1) = eval e1 env mem in
  let (v2,mem2) = eval e2 env mem1 in
  (Int((val2int v1) * (val2int v2)),mem2)


  |DIV (e1,e2) -> let (v1,mem1) = eval e1 env mem in
  let (v2,mem2) = eval e2 env mem1 in
  (Int((val2int v1) / (val2int v2)),mem2)


  |ISZERO e -> let (v1,mem1) = eval e env mem in
  if((val2int v1) = 0) then (Bool true,mem) else (Bool false,mem)


  |READ -> let x = read_int() in (Int x,mem)


  |IF (e1,e2,e3) -> let (v1,mem1) = eval e1 env mem in
  if((val2int v1) = 1) then eval e2 env mem1 else eval e3 env mem1


  |LET (x,e1,e2) -> let (v1,mem1) = eval e1 env mem in
  let l = new_location () in 
  let env1 = extend_env (x,(Loc l)) env in
  let mem2 = extend_mem (l,v1) mem1 in
  eval e2 env1 mem2


  |LETREC (f,x,e1,e2) -> 
  let l = new_location () in
  let env' = extend_env(f, (Loc l)) env in
  let mem' = extend_mem (l,RecProcedure(f,x,e1,env)) mem in 
  eval e2 env' mem'


  |PROC (x,e) -> (Procedure(x,e,env),mem)

  |CALL (e1,e2) -> 
  let l = new_location () in
  let (v1,mem1) = eval e1 env mem in
  let (v2,mem2) = eval e2 env mem1 in
  (
    match v1 with
    |Procedure (x,e,p_env) ->      
      let new_env = extend_env(x,Loc l) p_env in
      let new_mem = extend_mem(l,v2) mem2 in
      eval e new_env new_mem

    |RecProcedure(f,x,e,r_env) ->
      let new_env = extend_env(f,Loc l) r_env in    (* f -> f x e env *)
      let new_mem = extend_mem(l,v1) mem2 in

      let l2 = new_location () in    (* x -> v *)
      let new_env' = extend_env(x,Loc l2) new_env in
      let new_mem' = extend_mem(l2,v2) new_mem in
      eval e new_env' new_mem'

    |_ -> raise UndefinedSemantics
  )


  | NEWREF e -> let (v1,mem1) = eval e env mem in
  let new_loc = new_location () in
  let mem' = extend_mem (new_loc,v1) mem1 in
  (Loc new_loc ,mem')


  | DEREF e -> let (v1,mem1) = eval e env mem in
  ((apply_mem mem1 (val2int v1)),mem1)


  | SETREF (e1,e2) -> let (v1,mem1) = eval e1 env mem in
  let (v2,mem2) = eval e2 env mem1 in
  let new_mem = (extend_mem((val2int v1),v2) mem2) in
  (v2,new_mem)


  | SEQ (e1,e2) ->
  let (v1,mem1) = eval e1 env mem in
  eval e2 env mem1

  | BEGIN e ->
  eval e env mem
  
  and val2int v =
  match v with
  | Loc n -> n
  | Int n -> n
  | Bool b -> if (b = true) then 1 else 0
  | _ -> raise UndefinedSemantics






  (* driver code *)
  let run : program -> value
  =fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem)