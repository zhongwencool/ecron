%%% @private
-module(ecron_app).

-behaviour(application).
-include_lib("ecron.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ecron_sup:start_link().

stop(_State) ->
    ok.
