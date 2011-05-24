%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(wm_sched_airlines).
-export([init/1, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

%% --------------

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, Context) ->
  {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Context) ->
  {ok, Airlines} = sched_schedules_db:airlines(),
  JsonAirlines = lists:map(
    fun(Airline) -> sched_parser:integer_to_airline(Airline) end,
    lists:sort(Airlines)
  ),
  Json = {struct, [
    {status, <<"ok">>},
    {airlines, JsonAirlines}
  ]},
  {mochijson2:encode(Json), ReqData, Context}.
