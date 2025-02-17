-module(prop_ecron_basic_SUITE).
-include_lib("ecron/include/ecron.hrl").

%%% Common Test includes
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1, mail_delay/1]).
-compile(export_all).

-define(NAME, ?MODULE).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [
        basic,
        error_start_end_time,
        max_runtime_ms_aborted,
        max_runtime_ms_unlimited
    ].

groups() -> [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    error_logger:tty(true),
    ok.

init_per_testcase(_TestCase, Config) ->
    application:ensure_all_started(ecron),
    {ok, _Pid} = ecron:start_link(?NAME, []),
    Config.

basic(_Config) ->
    JobName = check_mail,
    MFA = {io, format, ["Mail checking~n"]},
    {ok, JobName} = ecron:add(?NAME, JobName, "0 0 8 * * 1-5", MFA, unlimited, unlimited, []),
    {ok, Result} = ecron:statistic(?NAME, JobName),
    [Result1] = ecron:statistic(?NAME),
    [] = ecron:statistic(ecron_not_found),
    ?assertEqual(Result1, Result, "ecron:statistic/2"),
    #{
        crontab :=
            #{
                day_of_month := '*',
                day_of_week := [{1, 5}],
                hour := "\b",
                minute := [0],
                month := '*',
                second := [0]
            } = CrontabSpec,
        end_time := {23, 59, 59},
        failed := 0,
        mfa := MFA,
        name := check_mail,
        next := Next,
        opts := [{singleton, true}, {max_count, unlimited}, {max_runtime_ms, unlimited}],
        start_time := {0, 0, 0},
        status := activate,
        type := cron
    } = Result,
    true = prop_ecron:check_cron_result(CrontabSpec, {0, 0, 0}, {23, 59, 59}, Next),
    ok = ecron:deactivate(?NAME, JobName),
    {ok, #{status := deactivate}} = ecron:statistic(?NAME, JobName),
    ok = ecron:delete(?NAME, JobName),
    {error, not_found} = ecron:statistic(?NAME, JobName),
    ok.

error_start_end_time(_Config) ->
    JobName = check_mail_with_time,
    MFA = {io, format, ["Mail checking~n"]},
    {error, invalid_time, _} = ecron:add_with_time(
        JobName, "0 0 8 * * 1-5", MFA, {12, 0, 0}, {11, 0, 0}
    ),
    {error, invalid_time, _} = ecron:add_with_time(
        JobName, "0 10 8,9 * * 1-5", MFA, {9, 20, 0}, {12, 0, 0}
    ),
    {error, invalid_time, _} = ecron:add_with_time(
        JobName, "0 10 8,9 * * 1-5", MFA, {109, 20, 0}, {12, 0, 0}
    ),
    {error, invalid_time, _} = ecron:add_with_time(
        JobName, "0 10 8,9 * * 1-5", MFA, {9, 20, 0}, 12
    ),
    {error, invalid_time, _} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, unlimited, 1),
    {ok, JobName} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, unlimited, unlimited),
    ok = ecron:delete(JobName),
    {ok, JobName} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, {8, 0, 0}, unlimited),
    ok = ecron:delete(JobName),
    {ok, JobName} = ecron:add_with_time(JobName, "0 10 8,9 * * 1-5", MFA, unlimited, {10, 0, 0}),
    ok = ecron:delete(JobName),
    ok.

max_runtime_ms_aborted(_Config) ->
    JobName = check_mail_with_max_runtime_ms,
    MFA = {?MODULE, mail_delay, [200]},
    {ok, JobName} = ecron:add(?NAME, JobName, "@every 1s", MFA, unlimited, unlimited, [
        {max_runtime_ms, 100}
    ]),
    {ok, Result} = ecron:statistic(?NAME, JobName),
    ?assertMatch(
        #{
            failed := 0,
            mfa := MFA,
            name := JobName,
            next := _,
            opts := [{singleton, true}, {max_count, unlimited}, {max_runtime_ms, 100}],
            start_time := {0, 0, 0},
            end_time := {23, 59, 59},
            status := activate,
            type := every
        },
        Result
    ),
    timer:sleep(1150),
    {ok, Result2} = ecron:statistic(?NAME, JobName),
    ?assertMatch(
        #{
            ok := 0,
            failed := 1,
            aborted := 1
        },
        Result2
    ),
    ok = ecron:delete(?NAME, JobName),
    ok.

mail_delay(Delay) ->
    timer:sleep(Delay),
    io:format("Mail delay: ~p~n", [Delay]),
    ok.

max_runtime_ms_unlimited(_Config) ->
    UnlimitedJob = check_mail_with_unlimited_runtime_ms,
    MFA = {?MODULE, mail_delay, [200]},
    {ok, UnlimitedJob} = ecron:add(?NAME, UnlimitedJob, "@every 1s", MFA, unlimited, unlimited, [
        {max_runtime_ms, unlimited}
    ]),
    {ok, Result} = ecron:statistic(?NAME, UnlimitedJob),
    ?assertMatch(
        #{
            failed := 0,
            mfa := MFA,
            name := UnlimitedJob,
            next := _,
            opts := [{singleton, true}, {max_count, unlimited}, {max_runtime_ms, unlimited}],
            start_time := {0, 0, 0},
            end_time := {23, 59, 59},
            status := activate,
            type := every
        },
        Result
    ),
    timer:sleep(1250),
    {ok, Result2} = ecron:statistic(?NAME, UnlimitedJob),
    ?assertMatch(
        #{
            ok := 1,
            aborted := 0,
            failed := 0
        },
        Result2
    ),
    ok = ecron:delete(?NAME, UnlimitedJob),
    Job300 = check_mail_with_300_runtime_ms,
    {ok, Job300} = ecron:add(?NAME, Job300, "@every 1s", MFA, unlimited, unlimited, [
        {max_runtime_ms, 300}
    ]),
    timer:sleep(1250),
    {ok, Result3} = ecron:statistic(?NAME, Job300),
    ?assertMatch(
        #{
            ok := 1,
            aborted := 0,
            failed := 0
        },
        Result3
    ),
    ok = ecron:delete(?NAME, Job300),
    ok.
