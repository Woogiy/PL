type program = exp
and exp =
| SKIP
| TRUE
| FALSE
| CONST of int
| VAR of var
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| LE of exp * exp
| EQ of exp * exp
| NOT of exp 
| IF of exp * exp * exp
| WHILE of exp * exp 
| LET of var * exp * exp
| PROC of var list * exp 
| CALLV of exp * exp list 
| CALLR of exp * var list
| ASSIGN of var * exp 
| RECORD of (var * exp) list 
| FIELD of exp * var
| ASSIGNF of exp * var * exp 
| READ of var
| PRINT of exp 
| SEQ of exp * exp
| BEGIN of exp
and var = string

type value = 
Int of int
| Bool of bool
| Unit
| Procedure of var list * exp * env
| Record of record
| Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
	match v with
	| Int n -> string_of_int n
	| Bool b -> string_of_bool b
	| Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
	| Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

	(* environment *)
	let empty_env = []
	let extend_env (x,v) e = (x,v)::e
	let rec apply_env e x = 
	match e with
	| [] -> raise (Failure (x ^ " is unbound in Env"))
	| (y,v)::tl -> if x = y then v else apply_env tl x

	(* memory *)
	let empty_mem = [] 
	let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
	let rec apply_mem m l = 
	match m with
	| [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
	| (y,v)::tl -> if l = y then v else apply_mem tl l

	let counter = ref 0
	let new_location () = counter:=!counter+1; (!counter)

	(* conversion of env to string *)
	let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
	(* conversion of mem to string *)
	let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		

	exception NotImplemented
	exception UndefinedSemantics
	(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
	let remove_garbage = ref false 

	let gc: env * mem -> mem
	= fun (env, mem) ->
	if (not !remove_garbage) then mem 
else 
	raise NotImplemented (* TODO *)

	let rec eval : program -> env -> mem -> (value * mem)
	=fun pgm env mem ->  
	match pgm with
	| READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)

	| PRINT e ->
	let v, mem' = eval e env mem in
	let _ = print_endline (value2str v) in
	(v, gc(env,mem')) (* Do not modify *) 

	| SKIP -> (Unit,mem)

	| TRUE -> (Bool true,mem)

	| FALSE -> (Bool false,mem)

	| CONST n -> (Int n,mem)

	| VAR x -> (Loc (apply_env env x), mem)

	| ADD (e1, e2) -> 
	let v1, mem1 = eval e1 env mem in 
	let v2, mem2 = eval e2 env mem1 in
	(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 + n2), mem2) 
		| _ -> raise UndefinedSemantics)     

	| SUB (e1, e2) -> 
	let v1, mem1 = eval e1 env mem in 
	let v2, mem2 = eval e2 env mem1 in
	(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 - n2), mem2) 
		| _ -> raise UndefinedSemantics)     

	| MUL (e1, e2) -> 
	let v1, mem1 = eval e1 env mem in 
	let v2, mem2 = eval e2 env mem1 in
	(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 * n2), mem2) 
		| _ -> raise UndefinedSemantics)     

	| DIV (e1, e2) -> 
	let v1, mem1 = eval e1 env mem in 
	let v2, mem2 = eval e2 env mem1 in
	(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 / n2), mem2) 
		| _ -> raise UndefinedSemantics)  

	| LE (e1, e2) ->
	let v1, mem1 = eval e1 env mem in 
	let v2, mem2 = eval e2 env mem1 in
	(match v1, v2 with
	| Int n1, Int n2 -> if(n1 <= n2) then (Bool true,mem2)
						 else (Bool false,mem2)
	| _ -> raise UndefinedSemantics)

	| EQ (e1, e2) ->
	let v1, mem1 = eval e1 env mem in 
	let v2, mem2 = eval e2 env mem1 in
	(match v1, v2 with
	| Int n1, Int n2 -> if(n1 <= n2) then (Bool false,mem2)
						 else (Bool true,mem2)
	| _ -> raise UndefinedSemantics)

	| NOT e ->
	let v1,mem1 = eval e env mem in
	(match v1 with
		| Bool b -> if b then (Bool false,mem) 
					 else(Bool true, mem)
		| _ -> raise UndefinedSemantics)

	| IF (e1,e2,e3) ->
	let v1,mem1 = eval e1 env mem in
	(match v1 with
		|Bool true  -> eval e2 env mem1
		|Bool false -> eval e3 env mem1
		| _ -> raise UndefinedSemantics)

	| WHILE (e1,e2) ->
	let v0,mem0 = eval e1 env mem in
	(match v0 with
		|Bool b -> if b then 
		(let v1,mem1 = eval e2 env mem in
		eval (WHILE(e1,e2)) env mem1)
		else eval SKIP env mem
		| _ -> raise UndefinedSemantics)

	|LET (x,e1,e2) ->
	let (v1,mem1) = eval e1 env mem in
  	let l = new_location () in 
  	let env1 = extend_env (x,l) env in
  	let mem2 = extend_mem (l,v1) mem1 in
  	eval e2 env1 mem2
	
	| PROC (xlist, e) -> (Procedure(xlist, e, env), mem)

	
	
	| CALLV  (e1,valListForFuntionCall) ->  
		let ( (Procedure (varListInFuntion,expr,envForFuntionCall)) ,mem') = eval e1 env mem in
		let (envforFun,envFormem) = callByValue varListInFuntion valListForFuntionCall env envForFuntionCall mem' 
		in eval expr envforFun envFormem
	
	| CALLR (e1,varList) ->	
	let ( (Procedure (varListInFuntion,expr,envForFunctionCall)) ,mem') = eval e1 env mem in
	let (envforFun,envFormem) = callByRef varListInFuntion varList env envForFunctionCall mem' 
	in eval expr envforFun envFormem
	


	| ASSIGN (v,e) ->
	let v1,mem1 = eval e env mem in
	let env1 = apply_env env v in
	let mem2 = extend_mem (env1,v1) mem1 in 
	v1,mem2
	

	| RECORD li ->
		(match li with
		|[] -> (Unit, mem)
		|_ -> (recording2record li env mem []))



	| FIELD (e,v) ->
			let (v1,mem1) = eval e env mem in
			let v2 = apply_mem mem1 (apply_env (record2ev v1) v) in 
			v2,mem
	

	| ASSIGNF (e1,v,e2)  ->
		let (v1,mem1) = eval e1 env mem in
			let (v2,mem2) = eval e2 env mem1 in 
				let mem3 = extend_mem ((apply_env (record2ev v1) v), v2) mem2 in 
				(v2,mem3)


	| SEQ (e1,e2) ->
	let v1,mem1 = eval e1 env mem in
	let v2,mem2 = eval e2 env mem1 in
		v2,mem2
	

	| BEGIN e ->
  	eval e env mem

	and val2int v =
  	match v with
  	| Loc n -> n
  	| Int n -> n
  	| Bool b -> if (b = true) then 1 else 0
  	| _ -> raise UndefinedSemantics

  	and record2ev r =
  	match r with
  	|Record l -> l
  	| _ -> raise UndefinedSemantics

  	
and callByValue varList valList rootenv targetenv mem =
	match (varList,valList) with 
	| ([],[]) ->  (targetenv,mem)
	| (hd1::tl1,hd2::tl2) -> 
			let newLoc = new_location () in
			let (valForvar,mem') = eval hd2 rootenv mem in
			let envForFuntionCall = extend_env (hd1,newLoc) targetenv in  
			let mem'' = extend_mem (newLoc,valForvar) mem' in callByValue tl1 tl2 rootenv envForFuntionCall mem''
	| ([],_) -> raise UndefinedSemantics
	| (_,[]) -> raise UndefinedSemantics

	and callByRef varList valList rootenv targetenv mem =
	match (varList,valList) with 
	| ([],[]) ->  (targetenv,mem)
	| (hd1::tl1,hd2::tl2) -> 
			let loc = apply_env rootenv hd2 in
			let envForFuntionCall = extend_env (hd1,loc) targetenv in callByRef tl1 tl2 rootenv envForFuntionCall mem
	| ([],_) -> raise UndefinedSemantics
| (_,[]) -> raise UndefinedSemantics

  	and recording2record li env mem last_record =
  	match li with
  	|[] -> (Record last_record,mem)
  	|(x,e)::tl ->
  	let v,mem1 = eval e env mem in
  	let l = new_location() in
  	let mem2 = extend_mem (l,v) mem1 in
  	let last_record' = (x,l)::last_record in 
  	(recording2record tl env mem2 last_record')

	let run : program -> bool -> bool -> unit 
	= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
	print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
	
	
