%%%-------------------------------------------------------------------
%%% @author Domin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Apr 2023 16:57
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Domin").
-export([start/0]).

start() ->
%%  register(pingpong, spawn(fun () -> ping_loop() end)),

%%  Ping = spawn(fun() -> ping_loop() end),
%%  Pong = spawn(fun() -> pong_loop() end),
%%  Ping ! {10, Pong}.
  register(ping, spawn(fun () -> ping_loop() end)),
  register(pong, spawn(fun () -> pong_loop() end)),

  play(5),
  start().
%%  pingpong ! 10.

play(N) ->
  ping ! N.
%%sleep(N) ->
%%  receive
%%    _ -> _
%%  after N ->
%%    _
%%  end.

pong_loop() ->
  receive
    0 -> io:format("[Pong] Value is 0, ending ping-pong~n");
    N ->
      timer:sleep(100),
      io:format("[Pong] Value is ~p, sending to [Ping], self PID = ~w ~n", [N, self()]),
      ping ! N - 1,

      pong_loop()
%%  after
%%    1000 -> _
  end.

ping_loop() ->
  receive
    0 -> io:format("[Ping] Value is 0, ending ping-pong~n");
    N ->
      timer:sleep(100),
      io:format("[Ping] Value is ~p, sending to [PONG], self PID = ~w ~n", [N, self()]),
      pong ! N - 1,
      ping_loop()
%%  after?
  end.
%% API

