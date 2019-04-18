type exp = 
	X | INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp
and environmnet = (exp * exp) list

let rec calculate : exp -> float
=	
	fun e ->
		calculate_in e []


	and calculate_in : exp -> environmnet -> float
	= fun e env ->
	match e with
	|X -> calculate_in (List.assoc X env) env
	|INT n -> (float_of_int n)
	|REAL n -> n
	|ADD (e1,e2) -> (calculate_in e1 env +. calculate_in e2 env)
	|SUB (e1,e2) -> (calculate_in e1 env -. calculate_in e2 env)
	|MUL (e1,e2) -> (calculate_in e1 env *. calculate_in e2 env)
	|DIV (e1,e2) -> (calculate_in e1 env /. calculate_in e2 env)
	|SIGMA(e1,e2,e3) -> 
	let env' = (X,e1)::env in
	if(calculate_in e1 env' <= calculate_in e2 env') then
		calculate_in e3 env' +. calculate_in (SIGMA(ADD(e1,INT 1),e2,e3)) []
	else 0.0
	|INTEGRAL(e1,e2,e3) ->
	let env' = (X,e1)::env in
	if(calculate_in e1 env' < calculate_in e2 env') then
		(calculate_in e3 env' *. 0.1) +. calculate_in (INTEGRAL(ADD(e1,REAL 0.1),e2,e3)) []
	else 0.0

	
	
	

let test t answer =
  let v = calculate t in
  (abs_float (v -. answer)) <= 0.5