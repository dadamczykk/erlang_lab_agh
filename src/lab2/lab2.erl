-module(lab2).
-author("Domin").


-export([funcia/0]).

%%Zdefiniuj funkcję anonimową, która w ciągu znaków podmieni
%%wszystkie „a” na „e”, „e” na „o”, pozostałe litery pozostawi bez zmian
%%Funcia = fun(Str) -> lists:map(fun (C) ->
%%        case C of
%%            $a -> $e;
%%            $e -> $o;
%%            _ -> C end
%%        end, Str) end.

%%Zdefiniuj funkcję anonimową, która policzy ile liczb w zadanej liście jest podzielnych przez 3.
%%Funcia2 = fun(L) -> L3 = [X || X <- L, X rem 3 == 0],
%%                    FFF = fun F(Acc, [T | H]) -> F(Acc + 1, H);
%%                              F(Acc, []) -> Acc end,
%%                    FFF(0, L3) end.

%%Stwórz funkcję anonimową, która policzy sumę cyfr w liczbie. Użyj do tego lists:foldl/3.
%%Funcia2 = fun(Num) -> FFF = fun F(0, T) -> T;
%%                                F(Num, Arr) -> F(Num div 10, Arr ++ [Num rem 10]) end,
%%                      lists:foldr(fun(Digit, Acc) -> Digit + Acc end, 0, FFF(Num, [])) end.