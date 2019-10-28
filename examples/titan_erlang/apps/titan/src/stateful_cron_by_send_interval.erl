-module(stateful_cron_by_send_interval).

-behaviour(gen_server).

-export([cancel_workday_stuff/0, cancel_monthly_stuff/0]).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {workday_job = undefined}).
-define(MonthlyJobName, monthly_stuff).

cancel_workday_stuff() ->
    gen_server:call(?MODULE, cancel_workday_stuff).

cancel_monthly_stuff() ->
    ecron:delete(?MonthlyJobName).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% If this process die, the crontab will auto delete.
    %% It's not necessary to delete job when terminate.
    {ok, JobRef} = ecron:send_interval("0 0 8 * * 1-5", self(), workday_stuff),
    Start = {{2020, 1, 1}, {0, 0, 0}},
    End = {{2022, 1, 1}, {0, 0, 1}},
    ecron:send_interval(?MonthlyJobName, "@monthly", self(), monthly_stuff, Start, End, []),
    {ok, #state{workday_job = JobRef}}.

handle_call(cancel_workday_stuff, _From, State = #state{workday_job = JobRef}) ->
    ecron:delete(JobRef),
    {reply, ok, State#state{workday_job = undefined}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(workday_stuff, State) ->
    do_send_workday_stuff(State),
    {noreply, State};
handle_info(monthly_stuff, State) ->
    do_send_monthly_stuff(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_send_workday_stuff(_State) ->
    io:format("Do workday stuff at ~p~n", [erlang:localtime()]).

do_send_monthly_stuff(_State) ->
    io:format("Do monthly stuff at ~p~n", [erlang:localtime()]).