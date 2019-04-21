let rec zipper l1 l2 =
	match (l1,l2) with
	|([],_) -> l2
	|(_,[]) -> l1
	|(hd1::tl1,hd2::tl2) -> hd1 :: hd2 :: zipper tl1 tl2;;