-module(mathx).
-author("Nolan Robidoux").
-timestamp("2020/08/08 19:14 UTC").

-export([mean/1, median/1, is_odd/1, is_even/1]).
-export([variance/1, stddev/1, cull/2]).

%% ====================================================================
%% API functions
%% ====================================================================
is_even(N) -> not is_odd(N).
is_odd(N) -> (N band 1) =:= 1.

mean(List) ->
    {N, Sum} = lists:foldl(fun(I, {N, Sum}) -> {N+1, Sum+I} end, {0,0}, List),
    Sum/N.

median(List) ->
    L = lists:sort(List),
    N = length(List),
    B = N div 2,

    case is_odd(N) of
        true -> lists:nth(B + 1, L);
        false -> 
            (lists:nth(B, L) + lists:nth(B + 1, L)) / 2.0
    end.

variance(Samples) ->
    Avg = mean(Samples),
    {N,X2} = lists:foldl(fun(X, {N, Out}) ->
                        {N+1, X*X + Out}
                        end,
                {0, 0}, Samples),
    {X2/(N-1)-(N*Avg*Avg/(N-1)), Avg}.

stddev(Samples) ->
    {V, M} = variance(Samples), 
    {math:sqrt(V), M}.

cull(Samples, NSigma) ->
    {Stddev,M} = stddev(Samples),
    UB = M + NSigma * Stddev,
    LB = M - NSigma * Stddev,

    lists:foldr(fun(E, In) ->
                    if 
                        (E < LB) -> In;
                        (E > UB) -> In;
                        true -> [E|In]
                    end
                end,
                [], Samples).

    
%% ====================================================================
%% Internal functions
%% ====================================================================