%%%-------------------------------------------------------------------
%%% @author Domin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2023 04:21
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Domin").

%% API
-export([start/0, stop/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_daily_mean/2,
  get_station_mean/2, get_moving_mean/3]).
-import(pollution, [create_monitor/0, add_station/3, add_value/5, remove_value/4,
get_daily_mean/3, get_one_value/4, get_moving_mean/4, get_station_mean/3]).

loop(Monitor) ->
  io:format("Waiting for command~n"),
  receive
    {PID, {add_station, Name, Coords}} ->
      Result = add_station(Name, Coords, Monitor),
      case Result of
        {error, _} -> PID ! Result, loop(Monitor);
        _ -> PID ! ok, loop(Result)
      end;
    {PID, {add_value, NamCor, Date, Type, Val}} ->
      Result = add_value(NamCor, Date, Type, Val, Monitor),
      case Result of
      {error, _} -> PID ! Result, loop(Monitor);
      _ -> PID ! ok, loop(Result)
      end;
    {PID, {remove_value, NamCor, Date, Type}} ->
      Result = remove_value(NamCor, Date, Type, Monitor),
      case Result of
        {error, _} -> PID ! Result, loop(Monitor);
        _ -> PID ! ok, loop(Result)
      end;
    {PID, {get_one_value, NamCor, Date, Type}} -> PID ! get_one_value(NamCor, Date, Type, Monitor), loop(Monitor);
    {PID, {get_daily_mean, Date, Type}} -> PID ! get_daily_mean(Type, Date, Monitor), loop(Monitor);
    {PID, {get_station_mean, NamCor, Type}} -> PID ! get_station_mean(NamCor, Type, Monitor), loop(Monitor);
    {PID, {get_moving_mean, NamCor, Type, Date}} -> PID ! get_moving_mean(Monitor, NamCor, Type, Date), loop(Monitor);
    stop -> ok
  end.

init() ->
  loop(create_monitor()).

start() ->
  io:format("Starting pollution server~n"),
  register(server, spawn_link(fun() -> init() end)).

stop() ->
  io:format("Stopping server~n"),
  server ! stop.

send(Msg) ->
  server ! {self(), Msg},
  receive
    Reply -> Reply
  end.

add_station(Name, Cor) -> send({add_station, Name, Cor}).
add_value(NamCor, Date, Type, Val) -> send({add_value, NamCor, Date, Type, Val}).
remove_value(NamCor, Date, Type) -> send({remove_value, NamCor, Date, Type}).
get_one_value(NamCor, Date, Type) -> send({get_one_value, NamCor, Date, Type}).
get_daily_mean(Type, Date) -> send({get_daily_mean, Date, Type}).
get_station_mean(NamCor, Type) -> send({get_station_mean, NamCor, Type}).
get_moving_mean(NamCor, Type, Date) -> send({get_moving_mean, NamCor, Type, Date}).