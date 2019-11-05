%%% @private
-module(ecron_sup).
-behaviour(supervisor).
-include("ecron.hrl").

-export([start_global/0, stop_global/0]).
-export([start_link/0, init/1]).

-define(LOCAL_WORKER, ecron_tick).
-define(MONITOR_WORKER, ecron_monitor).
-define(GLOBAL_WORKER, ecron_global).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_global() ->
    case supervisor:start_child(?MODULE,
        #{
            id => ?GLOBAL_WORKER,
            start => {ecron_tick, start_link, [{global, ?GLOBAL_WORKER}]},
            restart => temporary,
            shutdown => 1000,
            type => worker,
            modules => [?GLOBAL_WORKER]
        }) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, {{already_started, Pid}, _}} -> {ok, Pid}
    end.

stop_global() ->
    supervisor:terminate_child(?MODULE, ?GLOBAL_WORKER).

init([]) ->
    ?Job = ets:new(?Job, [named_table, set, public, {keypos, 2}]),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 60},
    ChildSpecs = [
        #{
            id => ?LOCAL_WORKER,
            start => {?LOCAL_WORKER, start_link, [{local, ?Ecron}]},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [?LOCAL_WORKER]
        }
        | node_monitor_worker()
    ],
    {ok, {SupFlags, ChildSpecs}}.

node_monitor_worker() ->
    node_monitor_worker(application:get_env(?Ecron, global_jobs, [])).

node_monitor_worker([]) -> [];
node_monitor_worker(_) ->
    [
        #{
            id => ?MONITOR_WORKER,
            start => {?MONITOR_WORKER, start_link, []},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [?MONITOR_WORKER]
        }
    ].
