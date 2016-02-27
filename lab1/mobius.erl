-module (mobius).
-export ([is_prime/1]).
-export ([prime_factors/1]).
-export ([find_square_multiples/2]).

is_prime(N) -> is_prime(N, trunc(math:sqrt(N))).

is_prime(1, _) -> true;
is_prime(2, _) -> true;
is_prime(_, 1) -> true;
is_prime(N, Cnt) ->
	if
		N rem Cnt =:= 0 -> false;
		N rem Cnt =/= 0 -> is_prime(N, Cnt - 1)
	end.


prime_factors(N) -> prime_factors(N, 2).
prime_factors(1, _) -> [];
prime_factors(N, Divider) when N rem Divider =/= 0 -> prime_factors(N, Divider + 1);
prime_factors(N, Divider) when N rem Divider == 0 -> [ Divider | prime_factors(N div Divider, 2) ].


is_square_multiple(N) ->
	Factors = prime_factors(N),
	UniqueFactors = sets:to_list(sets:from_list(Factors)),
	length(Factors) =/= length(UniqueFactors).


find_square_multiples(Count, MaxN) -> find_square_multiples(Count, MaxN, []).

find_square_multiples(Count, CurrentNumber, List) when length(List) =:= Count ->
	CurrentNumber + 1;
find_square_multiples(_, 2, _) -> fail;
find_square_multiples(Count, CurrentNumber, List) ->
	case is_square_multiple(CurrentNumber) of 
		true -> NewList = List ++ [CurrentNumber];
		false -> NewList = []
	end,
	find_square_multiples(Count, CurrentNumber - 1, NewList).
