%%% @private
-module(ecron_app).

-behaviour(application).

-include_lib("ecron.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ecron_telemetry_logger:attach(),
    ecron_sup:start_link().

stop(_State) ->
    rpc:abcast(nodes(), ?MONITOR_WORKER, {node(), ecron, stop}),
    ecron_telemetry_logger:detach(),
    ok.
