%%%-------------------------------------------------------------------
%%% @author Domin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2023 17:03
%%%-------------------------------------------------------------------
-module(qsort).
-author("Domin").

%% API
-export([random_elems/3, qs/1, less_than/2, grt_eq_than/2, compare_speeds/3, compare_speeds3/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot|Tail]) -> qs(less_than(Tail,Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) -> [rand:uniform(Max - Min + 1) + Min - 1 || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  [{Time1, List2}, {Time2, List2}] = [timer:tc(Fun1, [List]), timer:tc(Fun2, [List])],
  io:format("Function 1 time: ~p ms~n", [Time1]),
  io:format("Function 2 time: ~p ms~n", [Time2]).

compare_speeds3(List, Fun1, Fun2) ->
  [{Time1, List2}, {Time2, List2}] = [timer:tc(?MODULE, Fun1, [List]), timer:tc(?MODULE, Fun2, [List])],
  io:format("Function 1 time: ~p ms~n", [Time1]),
  io:format("Function 2 time: ~p ms~n", [Time2]).

%%qsort:compare_speeds(L2, fun qsort:qs/1, fun lists:sort/1).

%%NiceFun = lists:map(fun ($a) -> $e; ($e) -> $o end, "ala ma kote")

%%wszstko mabyć rekordem czy jakoś tak
%%ale wszystko przechowujemy w mapie w sensie już gotowe elementy
%%ChangeLetters = fun(String) ->
%%lists:map(fun(C) ->
%%case C of
%%$a -> $e;
%%$e -> $o;
%%_ -> C
%%end
%%end, String)
%%end.

%%
