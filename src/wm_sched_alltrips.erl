%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(wm_sched_alltrips).
-export([
  init/1,
  content_types_provided/2,
  malformed_request/2,
  to_json/2
]).

-include_lib("webmachine/include/webmachine.hrl").

%% --------------

init([]) ->
  {ok, undefined}.

content_types_provided(ReqData, Context) ->
  {[{"application/json", to_json}], ReqData, Context}.

malformed_request(ReqData, Context) ->
  QsOrigin = wrq:get_qs_value("org", ReqData),
  QsDestination = wrq:get_qs_value("dst", ReqData),
  QsDeparture = wrq:get_qs_value("dep", ReqData),
  QsAirlines = wrq:get_qs_value("air", ReqData),
  try
    {false, ReqData, {
      wm_sched_tools:qs_port_to_id(QsOrigin),
      wm_sched_tools:qs_port_to_id(QsDestination),
      wm_sched_tools:qs_departure_to_seconds(QsDeparture),
      wm_sched_tools:qs_airlines_to_filter(QsAirlines)
    }}
  catch
    error:_ -> {true, ReqData, Context}
  end.

to_json(ReqData, Context) ->
  {OrgID, DstID, DepSeconds, Airlines} = Context,
  {ok, AllTrips} = sched_network_db:all_trips(
    OrgID, DstID, DepSeconds, Airlines
  ),
  JsonOnD = {struct, [
    {org, wm_sched_tools:json_port(OrgID)},
    {dst, wm_sched_tools:json_port(DstID)},
    {dep, wm_sched_tools:json_date(DepSeconds)}
  ]},
  JsonResponse = {struct, [
    {ond, JsonOnD},
    {alltrips, wm_sched_tools:json_all_trips(AllTrips)}
  ]},
  {mochijson2:encode(JsonResponse), ReqData, Context}.
