%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(wm_sched_allpaths).
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
  try
    {false, ReqData, {
      wm_sched_tools:qs_port_to_id(wrq:get_qs_value("org", ReqData)),
      wm_sched_tools:qs_port_to_id(wrq:get_qs_value("dst", ReqData))
    }}
  catch
    error:_ -> {true, ReqData, Context}
  end.

to_json(ReqData, Context) ->
  {OrgID, DstID} = Context,
  {ok, AllPaths} = sched_network_db:all_paths(OrgID, DstID),
  JsonOnD = {struct, [
    {org, wm_sched_tools:json_port(OrgID)},
    {dst, wm_sched_tools:json_port(DstID)}
  ]},
  JsonResponse = {struct, [
    {ond, JsonOnD},
    {allpaths, wm_sched_tools:json_all_paths(AllPaths)}
  ]},
  {mochijson2:encode(JsonResponse), ReqData, Context}.
