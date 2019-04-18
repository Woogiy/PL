type formula = TRUE | FALSE
	|NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
	and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr
  

let rec eval : formula -> bool
= fun e ->
match e with
| TRUE -> true 
| FALSE -> false 
| NOT t ->
	(match t with 
	|TRUE -> false 
	|_ -> true)
| ANDALSO (t1,t2) -> ((eval t1) && (eval t2))
| ORELSE (t1,t2) -> ((eval t1) || (eval t2))
| IMPLY (t1,t2) ->
	(match t1,t2 with
		|TRUE,TRUE -> true
		|TRUE,FALSE -> false
		|_,_ -> true)
| LESS (t1,t2) -> (eval_expr t1) < (eval_expr t2)
and eval_expr: expr -> int
	= fun e2 ->
		match e2 with
		|NUM e2 -> e2
		|PLUS(t1,t2) -> (eval_expr t1) + (eval_expr t2)
		|MINUS(t1,t2) -> (eval_expr t1) - (eval_expr t2);;