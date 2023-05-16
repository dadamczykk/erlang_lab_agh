%%%-------------------------------------------------------------------
%%% @author Domin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2023 17:44
%%%-------------------------------------------------------------------
-module(distance).
-author("Domin").

%% API
-export([generate_points_list/1, compute_distances/2]).

generate_points_list(Length) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, Length)].

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1-X2, 2) + math:pow(Y1- Y2, 2)).

findMinDistance(PeopleLocations, LockerLocations) ->
  Distances = [{dist(P, L), P, L} || P <- PeopleLocations, L <- LockerLocations],
  lists:min(Distances).

findOneMing() ->
  receive
    {Pid, Person, LockerLocations} ->
      Md = findMinDistance([Person], LockerLocations),
      Pid ! Md
  end.
%%  findMinDistance([Person], LockerLocations).

%%gather_answers()

compute_distances(PeopleLocations, LockerLocations) ->
  lists:map(fun (Person) -> PID = spawn(fun () -> findOneMing() end), PID ! {self(), Person, LockerLocations} end, PeopleLocations),
  ans = lists:map(fun () -> receive
                              MD -> MD
                            end end, PeopleLocations).

