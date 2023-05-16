
-module(moul1).
%% API
-export([funcia/1, factorial/1, power/2, contains/2,
  duplicateElements/1, sumFloats/1, sumFloatsAcc/1, sumFloatsAcc/2]).

funcia (X) -> X + 1.

factorial(1) -> 1;
factorial(X) -> X * factorial(X-1).

power(A, 0) -> 1;
power(A, B) -> A * power(A, B - 1).

contains([H | _], H) -> true;
contains([], _) -> false;
contains([_ | T], Elem) -> contains(T, Elem).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H] ++ duplicateElements(T).

sumFloats([]) -> 0;
sumFloats([Num | Rest]) -> Num + sumFloats(Rest).

sumFloatsAcc(L) -> sumFloatsAcc(L, 0).

sumFloatsAcc([], Acc) -> Acc;
sumFloatsAcc([H | T], Acc) -> sumFloatsAcc(T, Acc + H).
