%%% @private
-module(ecron_sup).
-behaviour(supervisor).
-include("ecron.hrl").

-export([start_global/1, stop_global/1]).
-export([start_link/0, init/1]).

-define(LOCAL_WORKER, ecron_tick).
-define(GLOBAL_WORKER, ecron_global).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_global(Reason) ->
    case supervisor:start_child(?MODULE,
        #{
            id => ?GLOBAL_WORKER,
            start => {ecron_tick, start_link, [{global, ?Ecron}]},
            restart => temporary,
            shutdown => 1000,
            type => worker,
            modules => [?GLOBAL_WORKER]
        }) of
        {ok, Pid} ->
            Meta = #{action_ms => erlang:system_time(millisecond), reason => Reason},
            telemetry:execute(?GlobalUp, Meta, #{node => node()}),
            {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, {{already_started, Pid}, _}} -> {ok, Pid}
    end.

stop_global(Reason) ->
    case supervisor:terminate_child(?MODULE, ?GLOBAL_WORKER) of
        ok ->
            Meta = #{action_ms => erlang:system_time(millisecond), reason => Reason},
            telemetry:execute(?GlobalDown, Meta, #{node => node()});
        Err -> Err
    end.

init([]) ->
    ?Job = ets:new(?Job, [named_table, set, public, {keypos, 2}]),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 30
    },
    Local = #{
        id => ?LOCAL_WORKER,
        start => {?LOCAL_WORKER, start_link, [{local, ?Ecron}]},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [?LOCAL_WORKER]
    },
    Monitor = monitor_worker(),
    {ok, {SupFlags, [Local | Monitor]}}.

monitor_worker() ->
    Jobs = application:get_env(?Ecron, global_jobs, []),
    monitor_worker(Jobs).

monitor_worker([]) -> [];
monitor_worker(Jobs) ->
    [#{
        id => ?MONITOR_WORKER,
        start => {?MONITOR_WORKER, start_link, [{local, ?MONITOR_WORKER}, Jobs]},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [?MONITOR_WORKER]
    }].
