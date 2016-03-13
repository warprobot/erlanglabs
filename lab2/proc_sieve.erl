-module(proc_sieve).
-compile(export_all).

sieve(N, ReqPid, gather) ->
	receive 
		X -> ReqPid ! [N|X]
	end.

sieve(N, undefined)->
	receive
		{done, ReqPid} -> ReqPid ! [N];
		Num -> NextPid = case Num rem N of
			0 -> undefined ; 
			_ ->
				Pid = spawn(proc_sieve, sieve, []),
				Pid ! Num,
				Pid
			end,
			sieve(N, NextPid)
	end;
sieve(N, NextPid)->
	receive
		{done, ReqPid} ->
			NextPid ! {done, self()},
			sieve(N, ReqPid, gather);
		Num ->
			case Num rem N of
				0 -> undefined;
				_ -> NextPid ! Num
			end,
			sieve(N, NextPid)
	end.

sieve()->
	receive
		_N -> sieve(_N, undefined)
	end.

generate(MaxN)->
	Pid = spawn(?MODULE, sieve, []),
	[ Pid ! X || X <- lists:seq(2, MaxN) ], % [2..MaxN]
	Pid ! {done, self()},
	receive 
		Result -> Result
	end.

gen_print(MaxN) -> 
	lists:foreach(
		fun(Primes) -> io:format("~w ", [Primes]) end,
		generate(MaxN)
	).