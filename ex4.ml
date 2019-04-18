let rec iter n f a =
	match n with
	|0 -> a
	|_ -> f(iter (n-1) f a);;
