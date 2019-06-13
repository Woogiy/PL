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

	(* ex2 *)

	let rec make_new_mem origin_env origin_mem find_mem =
	match origin_env with
	| [] -> find_mem
	| (var,loc)::tl -> 
	let val1 = apply_mem origin_mem loc in
	let find_mem' = (loc,val1)::find_mem in
	make_new_mem tl origin_mem find_mem'


	let rec doCollection new_mem origin_mem =
	let new_mem' = collection new_mem origin_mem new_mem in
		if (List.length new_mem') = (List.length new_mem) 
			then new_mem'
	else doCollection new_mem' origin_mem
	
	and collection new_mem origin_mem final_mem =
	match new_mem with
	|[] -> final_mem
	|(l,v)::tl ->
		 (match v with
		|Int n -> collection tl origin_mem final_mem
		|Bool b -> collection tl origin_mem final_mem
		|Unit -> collection tl origin_mem final_mem
		|Procedure(varlist,e,env) -> collection tl origin_mem final_mem
		(* let final_mem' = proc2mem env origin_mem final_mem in
		collection tl origin_mem final_mem' *)
		|Loc l ->
		let val1 = (apply_mem origin_mem l) in
		let final_mem' = (l,val1)::final_mem in collection tl origin_mem final_mem'
		| Record l ->
		let record_mem = loclist2mem l origin_mem [] in
		let final_mem' = record_mem@final_mem in collection tl origin_mem final_mem' 
	)
	and loclist2mem locList origin_mem result =
	match locList with 
	| [] -> result
	| (var,loc)::tl -> let val1 = apply_mem origin_mem loc in 
	let result' = (loc,val1)::result in loclist2mem tl origin_mem result'

	(* and proc2mem procenv origin_mem result =
	match procenv with
	|[] -> result
	|(var,loc)::tl -> let val1 = apply_mem origin_mem loc in
	let result' = (loc,val1)::result in proc2mem tl origin_mem result' *)

	let gc: env * mem -> mem
	= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else let new_mem = make_new_mem env mem [] in doCollection new_mem mem

	
	



	(* ex1 *)
	let rec eval : program -> env -> mem -> (value * mem)
	=fun pgm env mem ->  
	match pgm with
	| READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
	let v, mem' = eval e env mem in
	let _ = print_endline (value2str v) in
	(v, gc(env,mem')) (* Do not modify *) 
	| SKIP -> (Unit,mem)
	| TRUE -> ((Bool true),mem)
	| FALSE -> ((Bool false),mem)
	| CONST n ->  ((Int n),mem)
	| VAR v ->  let loc = apply_env env v in 
	let v' = apply_mem mem loc in ((v',mem)) 
	| ADD (e1,e2) -> let (val1,mem') = eval e1 env mem in 
	let (val2,mem'') = eval e2 env mem' in
	(Int ((val2int val1)+(val2int val2)) , mem'' ) 

	| SUB (e1,e2)-> let (val1,mem') = eval e1 env mem in 
	let (val2,mem'') = eval e2 env mem' in
	(Int ((val2int val1)-(val2int val2)) , mem'' ) 

	| MUL (e1,e2)-> let (val1,mem') = eval e1 env mem in 
	let (val2,mem'') = eval e2 env mem' in
	(Int ((val2int val1)*(val2int val2)) , mem'' ) 

	| DIV (e1,e2)-> let (val1,mem') = eval e1 env mem in 
	let (val2,mem'') = eval e2 env mem' in
	(Int ((val2int val1)/(val2int val2)) , mem'' ) 

	| LE (e1,e2) -> let (val1,mem') = eval e1 env mem in	
	let (val2,mem'') = eval e2 env mem' in 
	let less = (val2int val1) <= (val2int val2) in
	if less then (Bool true,mem'') else(Bool false,mem'')

	| EQ (e1,e2) ->  let (val1,mem') = eval e1 env mem in	
	let (val2,mem'') = eval e2 env mem' in 
	(match (val1,val2) with
		|(Int n1 , Int n2) -> if n1 = n2 then (Bool true,mem'') else   (Bool false,mem'')
		|(Bool b1,Bool b2) ->  if b1 = b2 then (Bool true,mem'') else   (Bool false,mem'')
		|(Unit,Unit) -> (Bool true,mem'') 
		| _ -> (Bool false,mem'')
	)
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



	|LET (x,e1,e2) -> let (v1,mem1) = eval e1 env mem in
	let l = new_location () in 
	let env1 = extend_env (x,l) env in
	let mem2 = extend_mem (l,v1) mem1 in
	eval e2 env1 mem2

	
	| PROC (xlist, e) -> (Procedure(xlist, e, env), mem)
	
	| CALLV (e,elist) ->
	let v1,mem1 = eval e env mem in
	(match v1 with
		|Procedure (varlist,e',env_in_proc) ->
		let new_env,new_mem = callV_F varlist elist env env_in_proc mem1
	in eval e' new_env new_mem
|_ -> raise UndefinedSemantics)


	| CALLR (e,vlist) ->
	let v1,mem1 = eval e env mem in
	(match v1 with
		|Procedure(valist,e',env_in_proc) -> 
		let new_env,new_mem = callR_F valist vlist env env_in_proc mem1
	in eval e' new_env new_mem
|_ -> raise UndefinedSemantics)


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
	v2,mem3


	| SEQ (e1,e2) ->
	let v1,mem1 = eval e1 env mem in
	let v2,mem2 = eval e2 env mem1 in
	v2,mem2
	

	| BEGIN e ->
	eval e env mem

	
	and recording2record li env mem last_record =
	match li with
	|[] -> (Record last_record,mem)
	|(x,e)::tl ->
	let v,mem1 = eval e env mem in
	let l = new_location() in
	let mem2 = extend_mem (l,v) mem1 in
	let last_record' = (x,l)::last_record in 
	(recording2record tl env mem2 last_record')


	and val2int v =
	match v with
	| Loc n -> n
	| Int n -> n
	| _ -> raise UndefinedSemantics

	and record2ev r =
	match r with
	| Record l -> l
	| _ -> raise UndefinedSemantics

	and callV_F varlist elist origin_env proc_in_env mem =
	match(varlist,elist) with
	| ([],[]) ->  (proc_in_env,mem)
	| (hd1::tl1,hd2::tl2) -> 
	let l = new_location () in
	let (v,mem') = eval hd2 origin_env mem in
	let env' = extend_env (hd1,l) proc_in_env in  
	let mem'' = extend_mem (l,v) mem' in callV_F tl1 tl2 origin_env env' mem''
	| ([],_) -> raise UndefinedSemantics
	| (_,[]) -> raise UndefinedSemantics

	and callR_F varlist elist origin_env proc_in_env mem =
	match(varlist,elist) with
	| ([],[]) ->  (proc_in_env,mem)
	| (hd1::tl1,hd2::tl2) -> 
	let l = apply_env origin_env hd2 in
	let env' = extend_env (hd1,l) proc_in_env in  
	callR_F tl1 tl2 origin_env env' mem
	| ([],_) -> raise UndefinedSemantics
	| (_,[]) -> raise UndefinedSemantics



	let run : program -> bool -> bool -> unit 
	= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
	print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
