-module(ecron_telemetry_logger).

-include_lib("kernel/include/logger.hrl").
-include("ecron.hrl").

-define(Events, [?Success, ?Skipped, ?Aborted, ?Failure, ?Activate, ?Deactivate, ?Delete, ?GlobalUp, ?GlobalDown]).
%% API
-export([attach/0, detach/0]).
-define(TELEMETRY_HANDLE, ecron_telemetry_logger).
-define(LEVELS, [emergency, alert, critical, error, warning, notice, info, debug]).

attach() -> 
    LogLevel = application:get_env(ecron, log_level, notice),
    case lists:member(LogLevel, ?LEVELS) of
        true -> telemetry:attach_many(?TELEMETRY_HANDLE, ?Events, fun handle_event/4, LogLevel);
        false -> ok
    end.
    
detach() ->
    telemetry:detach(?TELEMETRY_HANDLE).

handle_event([ecron, Action], Event, #{name:= JobName}, Level) ->
    ?LOG(Level, Event#{action => Action, name => JobName});
handle_event([ecron, global, Action], Event, Meta, Level) ->
    Entry = maps:merge(Event, Meta),
    ?LOG(Level, Entry#{action => global_action(Action)}).        

global_action(down) -> global_down;
global_action(up) -> global_up.
