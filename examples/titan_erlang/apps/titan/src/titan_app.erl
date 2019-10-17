%%%-------------------------------------------------------------------
%% @doc titan public API
%% @end
%%%-------------------------------------------------------------------

-module(titan_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    titan_sup:start_link().

stop(_State) ->
    ok.
