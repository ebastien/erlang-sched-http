-module(wm_sched_tools).
-export([
  qs_departure_to_seconds/1,
  qs_port_to_id/1,
  qs_airline_to_id/1,
  qs_airlines_to_filter/1,
  qs_ports_to_path/1,
  json_port/1,
  json_date/1,
  json_datetime/1,
  json_all_paths/1,
  json_all_trips/1
]).

%% --------------

%% Convert a string of comma separated values to a list
csv_to_list(CSV) ->
  {Rem, CompleteTokens} = lists:foldl(
    fun(Char, {Buffer, Tokens}) ->
      if
        Char == $, -> {[], [lists:reverse(Buffer)|Tokens]};
        true       -> {[Char|Buffer], Tokens}
      end
    end,
    {[], []}, CSV
  ),
  lists:reverse([lists:reverse(Rem)|CompleteTokens]).

%% --------------

qs_departure_to_seconds(Departure) when is_list(Departure) ->
  DepInt = list_to_integer(Departure),
  DepYear = DepInt div 10000,
  DepMonth = (DepInt rem 10000) div 100,
  DepDay = DepInt rem 100,
  calendar:datetime_to_gregorian_seconds({{DepYear, DepMonth, DepDay}, {0, 0, 0}}).

%% --------------

qs_port_to_id(Port) when is_list(Port) ->
  sched_parser:port_to_integer(list_to_binary(Port)).

%% --------------

qs_airline_to_id(Airline) when is_list(Airline) ->
  sched_parser:airline_to_integer(list_to_binary(Airline)).

%% --------------

qs_airlines_to_filter(Airlines) ->
  case Airlines of
    AirlinesList when is_list(AirlinesList) ->
      AirlineIntsList = lists:map(
        fun(Airline) -> qs_airline_to_id(Airline) end,
        csv_to_list(AirlinesList)
      ),
      ordsets:from_list(AirlineIntsList);
    _ -> undefined
  end.

%% --------------

qs_ports_to_path(Ports) ->
  case Ports of
    PortsList when is_list(PortsList) ->
      lists:map(
        fun(Port) -> qs_port_to_id(Port) end,
        csv_to_list(Ports)
      );
    _ -> []
  end.

%% --------------

json_port(Port) ->
  {Lon, Lat} = sched_geography:coordinates(Port),
  {struct, [{apt, sched_parser:integer_to_port(Port)}, {lon, Lon*180.0/math:pi()}, {lat, Lat*180.0/math:pi()}]}.

%% --------------

json_date(Seconds) ->
  {{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Seconds),
  [Year, Month, Day, 0, 0].

%% --------------

json_datetime(Seconds) ->
  {{Year, Month, Day}, {Hour, Min, _}} = calendar:gregorian_seconds_to_datetime(Seconds),
  [Year, Month, Day, Hour, Min].

%% --------------

json_all_paths(AllPaths) ->
  json_all_paths(AllPaths, []).

json_all_paths([], Acc) -> Acc;
json_all_paths([Path|NextPaths], Acc) ->
  JsonPath = lists:map(fun(Port) -> json_port(Port) end, Path),
  NewAcc = [JsonPath|Acc],
  json_all_paths(NextPaths, NewAcc).

%% --------------

json_all_trips(AllTrips) ->
  json_all_trips(AllTrips, []).

json_all_trips([], Acc) -> Acc;
json_all_trips([{_, []}|NextAllTrips], Acc) ->
  json_all_trips(NextAllTrips, Acc);
json_all_trips([{Path, Trips}|NextAllTrips], Acc) ->
  JsonPath = lists:map(fun(Port) -> json_port(Port) end, Path),
  JsonTrips = {struct, [{pth, JsonPath}, {tps, json_trips(Trips, [])}]},
  NewAcc = [JsonTrips|Acc],
  json_all_trips(NextAllTrips, NewAcc).

json_trips([], Acc) -> Acc;
json_trips([Trip|NextTrips], Acc) ->
  JsonConnections = json_connections(Trip, []),
  NewAcc = [JsonConnections|Acc],
  json_trips(NextTrips, NewAcc).

json_connections([_], Acc) -> Acc;
json_connections([{Segment, Distance}|NextSegments], Acc) ->
  JsonSegment = {struct, [{seg, json_segment(Segment)}, {dst, round(Distance)}]},
  NewAcc = [JsonSegment|Acc],
  json_connections(NextSegments, NewAcc).

json_segment(Segment) ->
  {Airline, Board, Off, DepDateTime, ArrDateTime, FlightNumber} = Segment,
  {struct, [
    {air, sched_parser:integer_to_airline(Airline)},
    {flt, FlightNumber},
    {brd, sched_parser:integer_to_port(Board)},
    {dep, json_datetime(DepDateTime)},
    {off, sched_parser:integer_to_port(Off)},
    {arr, json_datetime(ArrDateTime)}
  ]}.
