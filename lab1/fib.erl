-module (fib).
-export ([fib_p/1]).
-export ([fib_g/1]).
-export ([tail_fib/1]).

% Non-tail recursive version
fib_p(0) -> 0;
fib_p(1) -> 1;
fib_p(N) when N > 1 -> fib_p(N - 1) + fib_p(N - 2).

% Non-tail recursive version
fib_g(N) when N =:= 0 -> N;
fib_g(N) when N =:= 1 -> N;
fib_g(N) when N > 1 -> fib_g(N - 1) + fib_g(N - 2).


tail_fib(N) when N >= 0 -> tail_fib_helper(N, 0, 1).
tail_fib_helper(0, Result, _) -> Result;
tail_fib_helper(N, Curr, Next) -> tail_fib_helper(N - 1, Next, Curr + Next).
