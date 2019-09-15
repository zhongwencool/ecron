-module(ecron_sup).
-behaviour(supervisor).

-export([add/1]).
-export([start_link/0, init/1]).

-define(CHILD_WORKER, ecron_job).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

add(JobSpec) ->
    case supervisor:start_child(?MODULE, [JobSpec]) of
        {error, {shutdown, Reason}} -> {error, Reason};
        Result -> Result
    end.

init([]) ->
    ets:new(ecron, [public, named_table, set]),
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},
    ChildSpecs = [
        #{
            id => ?CHILD_WORKER,
            start => {?CHILD_WORKER, start_link, []},
            restart => transient,
            shutdown => 1500,
            type => worker,
            modules => [?CHILD_WORKER]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
