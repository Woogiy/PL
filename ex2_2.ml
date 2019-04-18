type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list


let diff : ae*string -> ae
= fun (e1,e2) ->
match e1 with
|CONST n -> CONST 0
|VAR x -> if x = e2 then CONST 1 else VAR x
|POWER (x1,x2) -> 
if(x1 = e2) then
if (x2 = 2) then TIMES[CONST x2;VAR x1]
else TIMES[CONST x2;POWER(x1,x2-1)]
else
	CONST 0
	|TIMES l ->
	match l with
	|hd::tl -> hd
	|[] -> CONST 0

	(* |SUM l ->
	|hd::tl -> if hd = CONST 0 then []
	else SUM[diff(hd,e2); SUM(tl)]
	|[] -> CONST 0 *)