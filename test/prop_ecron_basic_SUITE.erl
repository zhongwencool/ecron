-module(prop_ecron_basic_SUITE).
-include_lib("ecron/include/ecron.hrl").

%%% Common Test includes
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).
-compile(export_all).

-define(NAME, ?MODULE).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [
        basic,error_start_end_time
    ].

groups() -> [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    error_logger:tty(true),
    ok.

init_per_testcase(_TestCase, Config) ->
    application:ensure_all_started(ecron),
    {ok, _Pid} = ecron:start_link({local, ?NAME}, []),
    Config.

basic(_Config) ->
    JobName  = check_mail,
    MFA = {io,format,["Mail checking~n"]},
    {ok, JobName} = ecron:add(?NAME, JobName, "0 0 8 * * 1-5", MFA, unlimited, unlimited, []),
    {ok, Result} = ecron:statistic(?NAME, JobName),
    [Result1] = ecron:statistic(?NAME),
    [] = ecron:statistic(ecron_not_found),
    ?assertEqual(Result1, Result, "ecron:statistic/2"),
    #{crontab :=
    #{day_of_month := '*',
        day_of_week := [{1,5}],
        hour := "\b",
        minute := [0],
        month := '*',
        second := [0]} = CrontabSpec,
        end_time := {23,59,59},
        failed := 0,
        mfa := MFA,
        name := check_mail,
        next := Next,
        opts := [{singleton,true},{max_count,unlimited}],
        start_time := {0,0,0},
        status := activate,type := cron} = Result,
    true = prop_ecron:check_cron_result(CrontabSpec, {0, 0, 0}, {23, 59, 59}, Next),
    ok = ecron:deactivate(?NAME, JobName),
    {ok, #{status := deactivate}} = ecron:statistic(?NAME, JobName),
    ok = ecron:delete(?NAME, JobName),
    {error, not_found} = ecron:statistic(?NAME, JobName),
    ok.

error_start_end_time(_Config) ->
    JobName  = check_mail_with_time,
    MFA = {io,format,["Mail checking~n"]},
    {error, invalid_time, _} = ecron:add_with_time(JobName, "0 0 8 * * 1-5", MFA, {12, 0, 0}, {11, 0, 0}),
    {error, invalid_time, _} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, {9, 20, 0}, {12, 0, 0}),
    {error, invalid_time, _} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, {109, 20, 0}, {12, 0, 0}),
    {error, invalid_time, _} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, {9, 20, 0}, 12),
    {error, invalid_time, _} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, unlimited, 1),
    {ok, JobName} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, unlimited, unlimited),
    ok = ecron:delete(JobName),
    {ok, JobName} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, {8, 0, 0}, unlimited),
    ok = ecron:delete(JobName),
    {ok, JobName} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, unlimited, {10, 0, 0}),
    ok = ecron:delete(JobName),
    ok.
