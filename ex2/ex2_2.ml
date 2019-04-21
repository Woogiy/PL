type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list


let rec diff : ae*string -> ae
= fun (e1,e2) ->
match e1 with
|CONST n -> CONST 0
|VAR x -> if x = e2 then CONST 1 else CONST 0
|POWER (x1,x2) -> 
if(x1 = e2) then
	if (x2 = 2) then TIMES[CONST x2;VAR x1]
	else TIMES[CONST x2;POWER(x1,x2-1)]
else
	CONST 0

|TIMES l -> (diff_times l e2)
|SUM l -> SUM (diff_sum l e2)

and diff_sum l var =
	match l with
	|hd::tl -> diff(hd,var) :: diff_sum tl var
	|[] -> []

and diff_times l var =
	match l with
	|hd::tl -> SUM([TIMES((diff(hd,var)::tl));TIMES((hd::[(diff_times tl var)]))])
	|[] -> CONST 0







let (|>) g f = f g
type r = (int * ((string * int) list)) list

let rec sum r1 r2 = 
    match r1, r2 with
    | _, [] -> r1
    | [], _ -> r2
    | (c1, xs1)::t1, (c2, xs2)::t2 ->
        if xs1 = xs2 then (c1 + c2, xs1)::(sum t1 t2)
        else if xs1 < xs2 then 
            (c1, xs1)::(sum t1 r2)
        else
            (c2, xs2)::(sum r1 t2)
let rec mult res a =

    match a with
    | CONST c1 -> 
        List.map (fun (c, xs) -> (c1 * c, xs)) res
	
    | VAR v ->
        mult res (POWER (v, 1))

    | POWER (x1, n1) ->
        let r = 
            List.map (fun (c, xs) ->
                let rec iter rlst = 
                    match rlst with
                    | [] -> [(x1, n1)]
                    | (x2, n2)::tl ->
                        if x1 = x2 then (x2, n1 + n2)::tl
                        else if x1 < x2 then (x1, n1)::rlst
                        else (x2, n2)::(iter tl) in
                (c, iter xs)) res in
        List.fold_left (fun res elem -> sum res [elem]) [] r  

    | SUM alst -> (
        match alst with
        | [] -> []
        | a::tl -> sum (mult res a) (mult res (SUM tl))
    )

    | TIMES alst ->
    (
        match alst with
        | [] -> res
        | a::tl -> mult (mult res a) (TIMES tl)
    )


let normalize a = 
    (mult [1, []] a)
    |> List.filter (fun (c, _) -> c <> 0)


let equals n1 a = 
    n1 = (normalize a)