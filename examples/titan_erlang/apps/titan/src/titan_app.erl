%%%-------------------------------------------------------------------
%% @doc titan public API
%% @end
%%%-------------------------------------------------------------------

-module(titan_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = titan_ecron_logger:attach(),
    titan_sup:start_link().

stop(_State) ->
    titan_ecron_logger:detach(),
    ok.
