type relationships = (string *string) list
type person = string

let graph = [("A", "B"); ("B", "C"); ("C", "B"); ("D", "A")]
let g1 = [ ("A", "E"); ("A", "H"); ("B", "C"); ("E", "G"); ("F", "G"); ("G", "B"); ("G", "D"); ("G", "E"); ("G", "F"); ("G", "H"); ("H", "A"); ("H", "B"); ("H", "C"); ("H", "F"); ("I", "B"); ("I", "H");]
let g2 = [ ("A", "C"); ("A", "I"); ("B", "H"); ("C", "B"); ("C", "F"); ("C", "G"); ("E", "H"); ("F", "H"); ("F", "I"); ("G", "H"); ("H", "B"); ("I", "B"); ("I", "C"); ("I", "D"); ("I", "F"); ("I", "H");]
let g3 = [ ("A", "F"); ("A", "H"); ("B", "D"); ("B", "E"); ("B", "F"); ("C", "B"); ("C", "H"); ("E", "E"); ("F", "D"); ("F", "E"); ("F", "G"); ("G", "B"); ("G", "E"); ("G", "G"); ("H", "B"); ("H", "D");]
let g4 = [ ("A", "A"); ("A", "G"); ("B", "I"); ("C", "A"); ("C", "C"); ("D", "E"); ("D", "G"); ("E", "D"); ("F", "C"); ("F", "F"); ("F", "I"); ("G", "F"); ("G", "G"); ("H", "B"); ("I", "G"); ("I", "H");]
let g5 = [ ("A", "A"); ("A", "G"); ("B", "I"); ("C", "A"); ("C", "C"); ("D", "E"); ("D", "G"); ("E", "D"); ("F", "C"); ("F", "F"); ("F", "I"); ("G", "F"); ("G", "G"); ("H", "B"); ("I", "G"); ("I", "H");]


let rec likes : relationships -> person -> int
= fun l person ->
	let l2 = (makeList l [] person) in
	let l3 = (likes_in l l2 [] person) in
	List.length l3

and likes_in = 
	fun l l2 l3 person -> 
	match l2 with
	|hd :: tl -> if((isMember l3 hd))
		then let l2' = (makeList l [] hd) in let l3' = (likes_in l l2' (l3@[hd]) hd) in (likes_in l tl l3' person)
		else likes_in l tl l3 person
	|[] -> l3

and makeList l l2 person = (* person이 좋아하는 사람들의 모음집 *)
	match l with
	|hd::tl -> if((fst hd) = person) then let l2' = l2@[snd hd] in makeList tl l2' person
	else makeList tl l2 person
	|[] -> l2

and isMember l person = 
	match l with
	|hd ::tl -> if (hd = person) then false else isMember tl person
	|[] -> true

and personLike l person =
	match l with
	|hd :: tl -> if (fst hd = person) then false else personLike tl person
	|[] -> true