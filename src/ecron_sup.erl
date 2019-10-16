%%% @private
-module(ecron_sup).
-behaviour(supervisor).
-include("ecron.hrl").

-export([start_link/0, init/1]).

-define(CHILD_WORKER, ecron_tick).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ?Job = ets:new(?Job, [named_table, set, public, {keypos, 2}]),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 60},
    ChildSpecs = [
        #{
            id => ?CHILD_WORKER,
            start => {?CHILD_WORKER, start_link, [{local, ?Ecron}]},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [?CHILD_WORKER]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
