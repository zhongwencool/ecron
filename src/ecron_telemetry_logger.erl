%%% @private
-module(ecron_telemetry_logger).

-include_lib("kernel/include/logger.hrl").
-include("ecron.hrl").

-define(Events, [
    ?Success, ?Skipped, ?Aborted, ?Crashed, ?Activate, ?Deactivate, ?Delete, ?GlobalUp, ?GlobalDown
]).
-define(ACTION_LEVEL, #{
    success => notice,
    activate => notice,
    deactivate => notice,
    delete => notice,
    skipped => error,
    aborted => error,
    crashed => error
}).
%% API
-export([attach/0, detach/0]).

attach() ->
    Level = application:get_env(ecron, log_level, all),
    logger:set_module_level([?MODULE], Level),
    Function = fun handle_event/4,
    Config = undefined,
    telemetry:attach_many(?MODULE, ?Events, Function, Config).

detach() ->
    logger:unset_module_level([?MODULE]),
    telemetry:detach(?MODULE).

handle_event([ecron, Action], Event, #{name := JobName}, _Config) ->
    Level = maps:get(Action, ?ACTION_LEVEL),
    ?LOG(Level, Event#{action => Action, name => JobName});
handle_event([ecron, global, Action], Event, Meta, _Config) ->
    Entry = maps:merge(Event, Meta),
    ?LOG(alert, Entry#{action => global_action(Action)}).

global_action(down) -> global_down;
global_action(up) -> global_up.
