let rec cartesian l1 l2 =
	match (l1,l2) with
	|(_,[]) -> []
	|([],_) -> []
	|(hd1::tl1,hd2::tl2) -> (hd1,hd2)::(cartesian [hd1] tl2)@(cartesian tl1 l2);;