%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the wm_sched application.

-module(wm_sched_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for wm_sched.
start(_Type, _StartArgs) ->
    wm_sched_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for wm_sched.
stop(_State) ->
    ok.
