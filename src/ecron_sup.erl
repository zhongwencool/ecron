%%% @private
-module(ecron_sup).

-behaviour(supervisor).

-include("ecron.hrl").

-export([start_global/1, stop_global/1]).
-export([start_link/0, init/1]).

-define(LOCAL_WORKER, ecron).
-define(GLOBAL_WORKER, ecron_global).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_global(Measurements) ->
    GlobalJobs = application:get_env(?Ecron, global_jobs, []),
    GlobalSpec = #{
        id => ?GLOBAL_WORKER,
        start => {ecron, start_link, [{global, ?GlobalJob}, GlobalJobs]},
        restart => temporary,
        shutdown => 1000,
        type => worker,
        modules => [?GLOBAL_WORKER]
    },
    case supervisor:start_child(?MODULE, GlobalSpec) of
        {ok, Pid} ->
            telemetry:execute(?GlobalUp, Measurements, #{self => node()}),
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, {{already_started, Pid}, _}} ->
            {ok, Pid}
    end.

stop_global(Measurements) ->
    case supervisor:terminate_child(?MODULE, ?GLOBAL_WORKER) of
        ok -> telemetry:execute(?GlobalDown, Measurements, #{self => node()});
        Err -> Err
    end.

init([]) ->
    LocalJobs = application:get_env(?Ecron, local_jobs, []),
    GlobalJobs = application:get_env(?Ecron, global_jobs, []),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 30
    },
    LocalSpec = #{
        id => ?LOCAL_WORKER,
        start => {?LOCAL_WORKER, start_link, [{local, ?LocalJob}, LocalJobs]},
        restart => permanent,
        shutdown => 1000,
        type => worker,
        modules => [?LOCAL_WORKER]
    },
    GlobalSpec =
        case application:get_env(?Ecron, global_jobs, []) of
            [] ->
                [];
            GlobalJobs ->
                [
                    #{
                        id => ?MONITOR_WORKER,
                        start => {?MONITOR_WORKER, start_link, [{local, ?MONITOR_WORKER}, GlobalJobs]},
                        restart => permanent,
                        shutdown => 1000,
                        type => worker,
                        modules => [?MONITOR_WORKER]
                    }
                ]
        end,
    {ok, {SupFlags, [LocalSpec | GlobalSpec]}}.
