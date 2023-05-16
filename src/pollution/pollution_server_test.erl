%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2019 12:50
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Wojciech Turek, Dominik Adamczyk").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
  pollution_server:start(),
  ?assertMatch(ok, pollution_server:add_station("Zakopane", {1, 1})),
  ?assertMatch(ok, pollution_server:add_station("Miasteczko cynamonek", {11, 11})),
  ?assertMatch({error, _}, pollution_server:add_station("Zakopane", {2,2})),
  ?assertMatch({error, _}, pollution_server:add_station("Krakow", {1,1})),
  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),

  timer:sleep(1100),
  Time2 = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM10", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, Time2, "PM1", 46.3)),
  ?assertNotMatch({error, _}, pollution_server:add_value( {1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3)),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 1", Time, "PM10", 36.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,1}, Time, "PM10", 36.3)),

  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_server:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3)),
  ?assertMatch({error, _}, pollution_server:add_value({1,2}, calendar:local_time(), "PM10", 46.3)),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),
  ?assertNotMatch({error, _}, pollution_server:remove_value("Stacja 1", Time, "PM10")),
  ?assertNotMatch({error, _}, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  ?assertNotMatch({error, _}, pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3)),
  ?assertEqual(46.3, pollution_server:get_one_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10"),
  ?assertNotMatch({error, _}, pollution_server:add_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10", 46.3)),
  ?assertEqual(46.3, pollution_server:get_one_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 46.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3),

  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:remove_value({1,2}, Time, "PM10")),
  ?assertMatch({error, _}, pollution_server:remove_value("Stacja 2", Time, "PM10")),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch(46.3, pollution_server:get_one_value("Stacja 1", Time, "PM10")),
  ?assertMatch(36.3, pollution_server:get_one_value("Stacja 1", Time, "PM1")),
  ?assertMatch(46.3, pollution_server:get_one_value({1,1}, Time, "PM10")),
  ?assertMatch(26.3, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10")),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  Time = calendar:local_time(),
  pollution_server:add_value("Stacja 1", Time, "PM10", 46.3),
  pollution_server:add_value("Stacja 1", Time, "PM1", 36.3),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3),

  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:get_one_value({1,1}, Time, "PM25")),
  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10")),
  ?assertMatch({error, _}, pollution_server:get_one_value("Stacja 2", Time, "PM1")),
  ?assertMatch({error, _}, pollution_server:get_one_value({1,2}, Time, "PM10")),
  pollution_server:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20),
  ?assertMatch(15.0, pollution_server:get_station_mean("Stacja 1", "PM10")),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  ?assertMatch(40/3, pollution_server:get_station_mean("Stacja 1", "PM10")),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,13}}, "PM10", 20),
  ?assertMatch(15.0, pollution_server:get_station_mean({1,1}, "PM10")),
  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_mean_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 1", "PM10")),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 1", "PM25")),
  ?assertMatch({error, _}, pollution_server:get_station_mean("Stacja 2", "PM25")),
  pollution_server:stop().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 3", {3,3}),
  pollution_server:add_station("Stacja 2", {2,2}),
  pollution_server:add_station("Stacja 1", {1,1}),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),
  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 10),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 22),
  ?assertMatch(15.0, pollution_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_server:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000),
  pollution_server:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000),
  pollution_server:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234),
  ?assertMatch(258.8, pollution_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_server:stop().



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
  pollution_server:start(),
  pollution_server:add_station("Stacja 2", {2,2}),
  pollution_server:add_station("Stacja 1", {1,1}),
  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,27})),
  pollution_server:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10),
  pollution_server:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20),

  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM25",{2023,3,27})),
  ?assertMatch({error, _}, pollution_server:get_daily_mean("PM10",{2023,3,29})),
  pollution_server:stop().

