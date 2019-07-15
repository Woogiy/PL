open M

(* exp environment : var -> exp *)
module EEnv = struct
  type t = var -> exp
  let empty = fun _ -> raise (Failure "Exp Env is empty")
  let extend (x,t) eenv = fun y -> if x = y then t else (eenv y)
  let find eenv x = eenv x
end

let rec hasX = 
  fun exp x2 ->
  match exp with
  |VAR x -> if((compare x2 x) == 0) then true else false
  |ADD(e1,e2) -> (hasX e1 x2) || (hasX e2 x2)
  |SUB(e1,e2) -> (hasX e1 x2) || (hasX e2 x2)
  |MUL(e1,e2) -> (hasX e1 x2) || (hasX e2 x2)
  |DIV(e1,e2) -> (hasX e1 x2) || (hasX e2 x2)
  |IF(e1,e2,e3) -> (hasX e1 x2) || (hasX e2 x2) || (hasX e3 x2)
  |PROC(x,e) -> (hasX e x2)
  |CALL(e1,e2) -> (hasX e1 x2) || (hasX e2 x2)
  |LET(x,e1,e2) -> if((compare x2 x) == 0)
      then false
      else (hasX e1 x) || (hasX e2 x)
  |LETREC(f,x,e1,e2) -> (hasX e1 x) || (hasX e2 x)
  |_-> false

let rec expandEEnv : exp -> EEnv.t -> exp
= fun exp eenv ->
match exp with
	|TRUE -> TRUE
	|FALSE -> FALSE
	|CONST n -> CONST n
	|VAR x -> (try EEnv.find eenv x with _ -> VAR x)
	|ADD(e1,e2) ->
    	let e1' =  expandEEnv e1 eenv in 
    	let e2' =  expandEEnv e2 eenv in  ADD(e1',e2')
  |SUB(e1,e2) ->
    	let e1' =  expandEEnv e1 eenv in 
    	let e2' =  expandEEnv e2 eenv in  SUB(e1',e2')
  |MUL(e1,e2)->
    	let e1' =  expandEEnv e1 eenv in 
    	let e2' =  expandEEnv e2 eenv in  MUL(e1',e2')
  |DIV (e1,e2) ->
    	let e1' =  expandEEnv e1 eenv in 
    	let e2' =  expandEEnv e2 eenv in  DIV(e1',e2')
  |ISZERO e ->
	    let e' =  expandEEnv e eenv in 
	    ISZERO(e')
	|READ -> READ
	|IF(e1,e2,e3) ->
		let e1' = expandEEnv e1 eenv in
		let e2' = expandEEnv e2 eenv in
		let e3' = expandEEnv e3 eenv in IF(e1',e2',e3')
  |LET(x,e1,e2) ->
  if(hasX e2 x) then (
    let e1' = expandEEnv e1 eenv in
  	let eenv' = EEnv.extend (x,e1') eenv in
  	let e2' = expandEEnv e2 eenv' in
  	e2') else let e2' = expandEEnv e2 eenv in LET(x,e1,e2')

  |LETREC(f,x,e1,e2) ->
  	let e1' = expandEEnv e1 eenv in
  	LETREC(f,x,e1',e2)
  |PROC (x,e) -> 
  	let e1' =  expandEEnv e eenv in  PROC(x,e1')
  |CALL (e1,e2) ->
  	let e1' = expandEEnv e1 eenv in 
		let e2' = expandEEnv e2 eenv in CALL(e1',e2')

let expand: exp -> exp 
= fun exp -> 
	expandEEnv exp EEnv.empty 

(* typeof: Do not modify this function *)
let typeof : exp -> typ 
= fun exp -> 
	let exp' = expand exp in 
	M.typeof exp'  