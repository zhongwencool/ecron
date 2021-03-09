-module(stateless_cron).

%% API
-export([add_every_4am_job/0]).
-export([add_limited_start_end_time_job/0]).
-export([add_limited_start_end_time_job2/0]).
-export([add_limited_run_count_job/0]).
-export([add_limited_run_count_job2/0]).
-export([delete_job/1]).
-export([job_stats/1, all_job_stats/0, predict_datetime_by_spec/2]).
-export([deactivate_job/1, activate_job/1]).

-export([inspect/1]).

%% Add job run at 04:00 everyday.
%% by ecron:add/3
add_every_4am_job() ->
    JobName = every_4am_job,
    MFA = {?MODULE, insepct, ["at 04:00 everyday."]},
    {ok, JobName} = ecron:add(JobName, "0 4 * * *", MFA).

%% Add job run at At minute 23 past every 2nd hour from 0 through 20
%% between {10, 0, 0} and {23, 59, 59}
%% by ecron:add_with_time/5
add_limited_start_end_time_job2() ->
    MFA = {?MODULE, inspect, ["at minute 23 past every 2nd hour from 0 through 20."]},
    Start = {{10, 0, 0}}, %% time or `unlimited`
    End = unlimited, %% time or `unlimited`
    ecron:add_with_time(make_ref(), "0 23 0-20/2 * * *", MFA, Start, End).

%% Add job run at 14~23:00-15-30-45 on day-of-month 1
%% between {10, 0, 0} and {22, 0, 0}
%% by ecron:add_with_time/5
add_limited_start_end_time_job() ->
    JobName = limited_start_end_datetime_cron_job,
    MFA = {?MODULE, inspect, ["at 14~23:00-15-30-45 on day-of-month 1."]},
    Start = {10, 0, 0}, %% time or `unlimited`
    End = {22, 15, 0}, %% time or `unlimited`
    {ok, JobName} = ecron:add_with_time(JobName, "0 */15 14-23 1 * *", MFA, Start, End).

%% Can only run 100 times at 22:00 on every day-of-week from Monday through Friday.
%% by ecron:add_with_count/3
add_limited_run_count_job() ->
    MFA = {?MODULE, inspect, ["at 22:00 on every day-of-week from Monday through Friday."]},
    ecron:add_with_count("0 22 * * 1-5", MFA, 100).

%% Can only run 100 times at minute 0 past hour 0 and 12 on day-of-month 1 in every 2nd month.
%% by ecron:add_with_count/4
add_limited_run_count_job2() ->
    MFA = {?MODULE, inspect, ["at minute 0 past hour 0 and 12 on day-of-month 1 in every 2nd month."]},
    JobName = limited_run_count_cron_job,
    ecron:add_with_count(ecron_local, JobName, "0 22 * * 1-5", MFA, 100).

%% Delete a specific task
delete_job(JobName) ->
    ok = ecron:delete(JobName).

%% MFA
inspect(Format) ->
    io:format(calendar:system_time_to_rfc3339(erlang:system_time(second)) ++" : "++ Format ++ "\n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debug Functions Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Pause job temporary
deactivate_job(JobName) ->
    ecron:deactivate(JobName).

%% Rerun job.
activate_job(JobName) ->
    ecron:activate(JobName).

%% Inspect specific statistic
job_stats(JobName) ->
    ecron:statistic(JobName).

%% Inspect all statistic
all_job_stats() ->
    ecron:statistic().

%% Predict latest N datetime.
predict_datetime_by_spec(Spec, N) ->
    ecron:parse_spec(Spec, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debug Functions End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%