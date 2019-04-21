let rec uniq l =
	let rec find_element e1 l =
	match l with
	|[] -> [e1]
	|hd::tl -> if(hd = e1) then hd::tl else hd::(find_element e1 tl)
	in
match l with
|[] -> []
|hd::tl ->  find_element hd (uniq tl)

let equals l1 l2 =
 let l1 = List.sort Pervasives.compare l1 in  
 let l2 = List.sort Pervasives.compare l2 in
 (Pervasives.compare l1 l2) = 0  

(* [2;3;4;2;3;4]
find_element 2 uniq [3;4;2;3;4]
find_element 2 find_element 3 uniq [4;2;3;4]
find_element 2 find_element 3 find_element 4 uniq [2;3;4]
find_element 2 find_element 3 find_element 4 find_element 2 uniq [3;4]
find_element 2 find_element 3 find_element 4 find_element 2 find_element 3 uniq [4]
find_element 2 find_element 3 find_element 4 find_element 2 find_element 3 find_element 4 []
find_element 2 find_element 3 find_element 4 find_element 2 find_element 3 [4]
find_element 2 find_element 3 find_element 4 find_element 2 [3;4]
find_element 2 find_element 3 find_element 4 find_element [2;3;4]
 *)