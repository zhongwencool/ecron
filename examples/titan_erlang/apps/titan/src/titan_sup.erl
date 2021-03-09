%%%-------------------------------------------------------------------
%% @doc titan top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(titan_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 10
    },
    CrontabName = titan_cron,
    Jobs = application:get_env(titan, crontab_jobs, []),
    Crontab =
        #{
            id => CrontabName,
            start => {ecron, start_link, [{local, CrontabName}, Jobs]},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [CrontabName]
    },
    WorkMods = [stateful_cron_by_send_after, stateful_cron_by_send_interval],
    ChildSpecs =
        [begin
             #{
                 id => Mod,
                 start => {Mod, start_link, []},
                 restart => permanent,
                 shutdown => 1000,
                 type => worker,
                 modules => [Mod]
             } end || Mod <- WorkMods],
    {ok, {SupFlags, [Crontab|ChildSpecs]}}.
