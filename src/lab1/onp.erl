-module('onp').

-export([onp/1, power/2, parse/2, parseLegal/2, parseLegal/1]).

%% 1 + 2 * 3 - 4 / 5 + 6 --> 1 2 3 * + 4 5 / - 6 +
%% 1 + 2 + 3 + 4 + 5 + 6 * 7 --> 1 2 + 3 + 4 + 5 + 6 7 * +
%% ( (4 + 7) / 3 ) * (2 - 19) --> 4 7 + 3 / 2 19 - *
%% 17 * (31 + 4) / ( (26 - 15) * 2 - 22 ) - 1 -- > 17 31 4 + * 26 15 - 2 * 22 - / 1 -

power(_, 0) -> 1;
power(A, B) when B -> A * power(A, B - 1).


parse({error, _}, H) -> list_to_integer(H);
parse({H, []}, _) -> H.

parseLegal(T) -> parseLegal(T, T).

parseLegal([], Out) -> list_to_integer(Out);
parseLegal([46 | _], Out) -> list_to_float(Out);
parseLegal([44 | _], Out) -> list_to_float(Out);
parseLegal([ _ | T], Out) -> parseLegal(T, Out).


onp(L) -> onp(string:tokens(L, " "), []).

onp([], [H]) -> H;
onp(["+" | T], [N1, N2 | T1]) -> onp(T, [N1 + N2 | T1]);
onp(["-" | T], [N1, N2 | T1]) -> onp(T, [N2 - N1 | T1]);
onp(["*" | T], [N1, N2 | T1]) -> onp(T, [N1 * N2 | T1]);
onp(["/" | T], [0, _ | _]) -> erlang:error({error, "Division by zero"});
onp(["/" | T], [N1, N2 | T1]) -> onp(T, [N2 / N1 | T1]);
onp(["sqrt" | T], [N1 | T1]) when N1 >= 0 -> onp(T, [math:sqrt(N1) | T1]);
onp(["sin" | T], [N1 | T1]) -> onp(T, [math:sin(N1) | T1]);
onp(["cos" | T], [N1 | T1]) -> onp(T, [math:cos(N1) | T1]);
onp(["tan" | T], [N1 | T1]) -> onp(T, [math:tan(N1) | T1]);
onp(["pow" | T], [N1, N2 | T1]) -> onp(T, [math:pow(N2, N1) | T1]);
%%onp([H | T], L) -> onp(T, [parse(string:to_float(H), H) | L]).
onp([H | T], L) -> onp(T, [parseLegal(H) | L]).