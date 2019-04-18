type exp = 
	X | INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp



let env = ref 0.0 
let rec calculate : exp -> float
=	
	let x = !env in
	fun e ->
	match e with
	|X -> x
	|INT n -> (float_of_int n)
	|REAL n -> n
	|ADD (e1,e2) -> (calculate e1 +. calculate e2)
	|SUB (e1,e2) -> (calculate e1 -. calculate e2)
	|MUL (e1,e2) -> (calculate e1 *. calculate e2)
	|DIV (e1,e2) -> (calculate e1 /. calculate e2)
	|SIGMA(e1,e2,e3) ->
	if(calculate e1 <= calculate e2) then
		(env := (calculate e1); (calculate e3 +. calculate (SIGMA(ADD (e1,INT 1),e2,e3))))
	else (env := 0.0; 0.0)
	|INTEGRAL(e1,e2,e3) ->
	if(calculate e1 <= calculate e2) then
		(env := (calculate e1); calculate (MUL(e3,REAL 0.1)) +.calculate (SIGMA(ADD (e1,INT 1),e2,e3)))
	else (env := 0.0; 0.0)

	calculate(INTEGRAL(REAL 1.0,REAL 10.0,SUB(MUL(X,X),INT 1)))