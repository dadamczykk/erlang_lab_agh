%%%-------------------------------------------------------------------
%%% @author Domin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. May 2023 12:20
%%%-------------------------------------------------------------------
-module(pow).
-behavior(gen_server).
-author("Domin").

%% API
-export([start_link/0, step/0, read/0, close/0, crash/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, set_value/1]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE},?MODULE,2,[]).
init(N)        -> {ok,N}.

%% INTERFEJS KLIENT -> SERWER %%
step()      -> gen_server:cast(?MODULE,step).
read()      -> gen_server:call(?MODULE,read).
close()     -> gen_server:call(?MODULE,terminate).
crash()     -> gen_server:cast(?MODULE,crash).
set_value(New_value) -> gen_server:cast(?MODULE, {set_value, New_value}).

%% OBSŁUGA WIADOMOŚCI %%
handle_cast(step, N) -> {noreply, N*N};
handle_cast(crash, N) -> no:exist(), {noreply, N};
handle_cast({set_value, New_value}, _N) -> {noreply, New_value}.

handle_call(read,_From, N)      -> {reply, N, N};
handle_call(terminate,_From,N) -> {stop, normal, ok, N}.

handle_info(Other, State) ->
  io:format("Received an unspecified message: ~p~n", [Other]),
  {noreply, State}.

terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.