let rec gcd n m =
	if (n < m) then gcd m n
	else if  (m < 0) then failwith "invalid input"
	else match m with
		|0 -> n
		|_ -> gcd m (n - m*(n/m));;
