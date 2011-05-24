-module(wm_sched_setup).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, wm_sched_setup}, wm_sched_setup, [], []).

init(_Args) ->
  io:format("~nInitializing travel solutions server...~n"),
  ok = sched_schedules_db:load(),
  ok = sched_network_db:load(),
  io:format("~nDone.~n"),
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
