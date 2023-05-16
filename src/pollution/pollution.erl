%%%-------------------------------------------------------------------
%%% @author Domin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2023 18:03
%%%-------------------------------------------------------------------
-module(pollution).
-author("Domin").

%% API
-export([add_station/3, create_monitor/0, get_id/2, add_value/5, remove_value/4, get_one_value/4, perform_test/0,
  get_station_mean/3, generate_measurements/5, get_moving_mean/4, get_daily_mean/3]).
-include("../pollution.hrl").

create_monitor() ->
  #monitor{
    cords = maps:new(),
    names = maps:new(),
    stations = maps:new(),
    id = 0
  }.


add_station(Name, Cord, Monitor) ->
  case {maps:is_key(Name, Monitor#monitor.names), maps:is_key(Cord, Monitor#monitor.cords)} of
    {true, _} ->
      {error, Monitor};
    {_, true} ->
      {error, Monitor};
    _ -> New_station = #station{
      name = Name,
      cord = Cord,
      measurements = maps:new()
    },
      #monitor{
      cords = maps:put(Cord, Monitor#monitor.id, Monitor#monitor.cords),
      names = maps:put(Name, Monitor#monitor.id, Monitor#monitor.names),
      stations = maps:put(Monitor#monitor.id, New_station, Monitor#monitor.stations),
      id = Monitor#monitor.id + 1
    }
  end.

get_id(Monitor, {X, Y}) ->
  Out = maps:find({X, Y}, Monitor#monitor.cords),
  case Out of
    {ok, Val} -> Val;
    _ -> {error, station_not_exist}
  end;
get_id(Monitor, Name) ->
  Out = maps:find(Name, Monitor#monitor.names),
  case Out of
    {ok, Val} -> Val;
    _ -> {error, station_not_exist}
  end.

get_station(Monitor, NamCor) ->
  Id = get_id(Monitor, NamCor),
  case Id of
    {error, _} -> Id;
    _ -> maps:get(Id, Monitor#monitor.stations)
  end.


parse_date(D) ->
  D.
%%  {Day, {Hour, _, _}} = D,
%%  {Day, Hour}.

get_measurement(Station, Date) ->
  maps:get(Date, Station#station.measurements, #measurement{}).

add_measurement(Monitor, NamCor, Date, Measurement, Station) ->
  New_Station = Station#station{measurements = maps:put(Date, Measurement, Station#station.measurements)},
  Monitor#monitor{stations = maps:update(get_id(Monitor, NamCor), New_Station, Monitor#monitor.stations)}.

add_value(NamCor, D, Type, Value, Monitor) ->
  Station = get_station(Monitor, NamCor),
  case Station of
    {error, _} -> Station;
    _ ->
    Date = parse_date(D),
    Measurement = get_measurement(Station, Date),
    case Type of
      "PM10" ->
        if Measurement#measurement.pm10 == undefined ->
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{pm10=Value}, Station);
          true ->
%%            io:fwrite("Measurement of pm10 is already saved for this date. Monitor will not be changed~n"),
            {error, measurement_already_in_database}
        end;
      "PM25" ->
        if Measurement#measurement.pm2_5 == undefined ->
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{pm2_5=Value}, Station);
          true ->
            io:fwrite("Measurement of 2_5 is already saved for this date. Monitor will not be changed~n"),
            {error, Monitor}
        end;
      "TEMPERATURE" ->
        if Measurement#measurement.temperature == undefined ->
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{temperature=Value}, Station);
          true ->
            io:fwrite("Measurement of temperature is already saved for this date. Monitor will not be changed~n"),
            {error, Monitor}
        end;
      "PRESSURE" ->
        if Measurement#measurement.pressure == undefined ->
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{pressure=Value}, Station);
          true ->
            io:fwrite("Measurement of pressure is already saved for this date. Monitor will not be changed~n"),
            {error, Monitor}
        end;
      "WINDSPEED" ->
        if Measurement#measurement.wind_speed == undefined ->
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{wind_speed=Value}, Station);
          true ->
            io:fwrite("Measurement of wind_speed is already saved for this date. Monitor will not be changed~n"),
            {error, Monitor}
        end;
        "HUMIDITY" ->
        if Measurement#measurement.humidity == undefined ->
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{humidity=Value}, Station);
          true ->
            io:fwrite("Measurement of humidity is already saved for this date. Monitor will not be changed~n"),
            {error, Monitor}
        end;
        _ ->
          Previous = Measurement#measurement.any,
          add_measurement(Monitor, NamCor, Date, Measurement#measurement{any = Previous ++ [{Type, Value}]}, Station)
    end
  end.

remove_value(NamCor, D, Type, Monitor) ->
  Station = get_station(Monitor, NamCor),
  case Station of
    {error, _} -> Station;
    _ ->
    Date = parse_date(D),
    Measurement = get_measurement(Station, Date),
    case Type of
      "PM10" ->
        if Measurement#measurement.pm10 == undefined -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{pm10=undefined}, Station) end;
      "PM25" ->
        if Measurement#measurement.pm2_5 == undefined -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{pm2_5=undefined}, Station) end;
      "TEMPERATURE" ->
        if Measurement#measurement.temperature == undefined -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{temperature=undefined}, Station) end;
      "PRESSURE" ->
        if Measurement#measurement.pressure == undefined -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{pressure=undefined}, Station) end;
      "WINDSPEED" ->
        if Measurement#measurement.wind_speed == undefined -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{wind_speed=undefined}, Station) end;
      "HUMIDITY" ->
        if Measurement#measurement.humidity == undefined -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{humidity=undefined}, Station) end;
      _ ->
        Previous = Measurement#measurement.any,
        Filtered_prev=lists:filter(fun ({Name, _}) ->  Name /= Type end, Previous),
        PreviousLen = length(Previous),
        FilteredLen = length(Filtered_prev),
        if PreviousLen == FilteredLen -> {error,  measurement_not_recorded};
          true ->
        add_measurement(Monitor, NamCor, Date, Measurement#measurement{any = Filtered_prev}, Station) end
    end
  end.

get_one_value(NamCor, D, Type, Monitor) ->
  Date = parse_date(D),
  Station = get_station(Monitor, NamCor),

  case Station of
    {error, _} -> Station;
    _ ->
      Measurement = get_measurement(Station, Date),
      VAL = case Type of
        "PM10" ->
          Measurement#measurement.pm10;
        "PM25" ->
          Measurement#measurement.pm2_5;
        "TEMPERATURE" ->
          Measurement#measurement.temperature;
        "PRESSURE" ->
          Measurement#measurement.pressure;
        "WINDSPEED" ->
          Measurement#measurement.wind_speed;
        "HUMIDITY" ->
          Measurement#measurement.humidity;
        _ ->
          L = lists:filter(fun({Name, _}) -> Name == Type end, Measurement#measurement.any),
          if L == [] ->
            undefined;
            true ->
              [{_, V} | _] = L,
              V
          end
      end,
    case VAL of
      undefined -> {error, measurement_not_recorded};
      _ -> VAL
    end
  end.

get_average([]) -> io:fwrite("No measurments of given type"), 0;
get_average(L) -> lists:sum(L) / length(L).

get_station_mean(NamCor, Type, Monitor) ->
  Station = get_station(Monitor, NamCor),
  case Station of
    {error, _} -> Station;
    _ ->
      Values = case Type of
        "PM10" ->
          lists:filter(fun (V) -> V /= undefined end,
            maps:fold(fun(_, Val, Acc) ->
              Acc ++ [Val#measurement.pm10]
            end, [], Station#station.measurements));
        "PM25" ->
          lists:filter(fun (V) -> V /= undefined end,
            maps:fold(fun(_, Val, Acc) ->
              Acc ++ [Val#measurement.pm2_5]
                      end, [], Station#station.measurements));
        "TEMPERATURE" ->
          lists:filter(fun (V) -> V /= undefined end,
            maps:fold(fun(_, Val, Acc) ->
              Acc ++ [Val#measurement.temperature]
                      end, [], Station#station.measurements));
        "PRESSURE" ->
          lists:filter(fun (V) -> V /= undefined end,
            maps:fold(fun(_, Val, Acc) ->
              Acc ++ [Val#measurement.pressure]
                      end, [], Station#station.measurements));
        "WINDSPEED" ->
          lists:filter(fun (V) -> V /= undefined end,
            maps:fold(fun(_, Val, Acc) ->
              Acc ++ [Val#measurement.wind_speed]
                      end, [], Station#station.measurements));
        "HUMIDITY" ->
          lists:filter(fun (V) -> V /= undefined end,
            maps:fold(fun(_, Val, Acc) ->
              Acc ++ [Val#measurement.humidity]
                      end, [], Station#station.measurements));
        _ ->
          lists:filter(fun (V) -> V /= {error, measurement_not_recorded} end,
            maps:fold(fun(Key, _, Acc) ->
    %%          {Day, Hour} = Key,
              V = get_one_value(NamCor, Key, Type, Monitor),
                  Acc ++ [V]
              end, [], Station#station.measurements))
               end,
      case Values of
        [] -> {error, no_measurements_for_this_type_and_station};
        _ -> get_average(Values)
      end
  end.


get_daily_mean(Type, Date, Monitor) ->
  Measurements = maps:filter(fun({{D, _},_}, _) -> D == Date end, maps:fold(fun(Key, Station, Acc) ->
                            maps:fold(fun(SubKey, Value, InnerAcc) ->
                                           maps:put({SubKey, Key}, Value, InnerAcc)
                                           end, Acc, Station#station.measurements)
                            end, #{}, Monitor#monitor.stations)),
  List = case Type of
    "PM10" ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) -> [Val#measurement.pm10 | Acc] end, [], Measurements));
    "PM25" ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) -> [Val#measurement.pm2_5 | Acc] end, [], Measurements));
    "TEMPERATURE" ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) -> [Val#measurement.temperature | Acc] end, [], Measurements));
    "PRESSURE" ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) -> [Val#measurement.pressure | Acc] end, [], Measurements));
    "WINDSPEED" ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) -> [Val#measurement.wind_speed | Acc] end, [], Measurements));
    "HUMIDITY" ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) -> [Val#measurement.humidity | Acc] end, [], Measurements));
    _ ->
      lists:filter(fun(Elem) -> Elem /= undefined end,
        maps:fold(fun(_, Val, Acc) ->
          [{_, V} | _] = lists:filter(fun({Name, _}) -> Name == Type end, Val#measurement.any),
          [V | Acc] end, [], Measurements))
  end,
  case List of
    [] -> {error, no_measurements_for_this_date};
    _ -> get_average(List)
  end.

subtract_time(Datetime, Seconds) ->
   calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Datetime) - Seconds).

get_moving_mean(Monitor, NamCor, Type, Datetime) ->
  get_moving_mean(Monitor, NamCor, Type, Datetime, 24, 0, 0).
get_moving_mean(_, _, _, _, 0, _, 0) ->
  io:fwrite("No measurements found in past 24 hours");
get_moving_mean(_, _, _, _, 0, Numerator, Denominator) ->
  Numerator / Denominator;
get_moving_mean(Monitor, NamCor, Type, Datetime, Weight, Numerator, Denominator) ->

  Val = get_one_value(NamCor, Datetime, Type,Monitor),
  if Val == undefined ->
    get_moving_mean(Monitor, NamCor, Type, subtract_time(Datetime, 3600),
      Weight - 1, Numerator, Denominator);
    true ->
      get_moving_mean(Monitor, NamCor, Type, subtract_time(Datetime, 3600),
        Weight - 1, Numerator + Val * Weight, Denominator + Weight)
  end.

generate_measurements(Monitor, _, _, _, 0) ->
  Monitor;
generate_measurements(Monitor, NamCor, Type, Datetime, HowManyPrev) ->
  NewMon = add_value(Monitor, NamCor, Datetime, Type, rand:uniform(99)),
  generate_measurements(NewMon, NamCor, Type, subtract_time(Datetime, 3600), HowManyPrev - 1).

perform_test()->
  M = pollution_server:add_station("Stacja 1", {1,1}, pollution_server:create_monitor()),
  Time = calendar:local_time(),
  M1 = pollution_server:add_value("Stacja 1", Time, "PM10", 46.3, M),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3, M1).
%%  P1 = pollution:create_monitor(),
%%  T1 = calendar:local_time(),
%%  T2 = {{11,11,11},{11,11,11}},
%%  T3 = {{11,11,11},{22,22,22}},
%%  {D, {H, M, S}} = T2,
%%  P2 = pollution:add_station(P1, "Zakopane", {12, 34}),
%%  P3 = pollution:add_value(P2, "Zakopane", T1, pm10, 11),
%%  P4 = pollution:add_value(P3, {12,34}, T2, antek, 20),
%%  P5 = pollution:add_value(P4, {12,34}, T3, antek, 11),
%%  P6 = pollution:add_station(P5, "Krakow", {11, 22}),
%%  P7 = pollution:add_value(P6, "Krakow",T1, pm10, 100),
%%  P8 = pollution:add_value(P7, "Krakow", {D, {H+5, 0, 0}}, antek, 34),
%%  P9 = generate_measurements(P8, "Zakopane", koza, T1, 24),
%%  P10 = generate_measurements(P9, "Zakopane", pm10, T1, 25),
%%  io:format("value~w~n", [get_moving_mean(P9, {12,34}, koza, T1)]),
%%  get_station(P10, "Zakopane").

%%multiply2(X) -> 4*X.

%%  Val = get_station_mean(P8, "Zakopane", pm2_5),
%%  get_daily_mean(P8, {11,11,11}, antek).




