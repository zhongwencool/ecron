-module(prop_ecron_basic_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    mail_delay/1
]).
-export([basic/1, error_start_end_time/1, max_runtime_ms_aborted/1, max_runtime_ms_unlimited/1]).

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
    {ok, _Pid} = ecron:start_link(?NAME),
    Config.

basic(_Config) ->
    JobName = check_mail,
    MFA = {io, format, ["Mail checking~n"]},
    {ok, JobName} = ecron:create(JobName, "0 0 8 * * 1-5", MFA, #{register => ?NAME}),
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
        crashed := 0,
        mfa := MFA,
        name := check_mail,
        next := Next,
        opts := [{singleton, false}, {max_count, unlimited}, {max_runtime_ms, unlimited}],
        start_time := {0, 0, 0},
        status := activate,
        type := cron
    } = Result,
    true = prop_ecron:check_cron_result(CrontabSpec, {0, 0, 0}, {23, 59, 59}, Next),
    ok = ecron:reload(?NAME),
    ok = ecron:deactivate(?NAME, JobName),
    {ok, #{status := deactivate}} = ecron:statistic(?NAME, JobName),
    ok = ecron:delete(?NAME, JobName),
    {error, not_found} = ecron:statistic(?NAME, JobName),
    ok.

error_start_end_time(_Config) ->
    JobName = check_mail_with_time,
    MFA = {io, format, ["Mail checking~n"]},
    {error, invalid_time, _} = ecron:create(
        JobName, "0 0 8 * * 1-5", MFA, #{start_time => {12, 0, 0}, end_time => {11, 0, 0}}
    ),
    {error, invalid_time, _} = ecron:create(
        JobName, "0 10 8,9 * * 1-5", MFA, #{start_time => {9, 20, 0}, end_time => {12, 0, 0}}
    ),
    {error, invalid_time, _} = ecron:create(
        JobName, "0 10 8,9 * * 1-5", MFA, #{start_time => {109, 20, 0}, end_time => {12, 0, 0}}
    ),
    {error, invalid_time, _} = ecron:create(
        JobName, "0 10 8,9 * * 1-5", MFA, #{start_time => {9, 20, 0}, end_time => 12}
    ),
    {error, invalid_time, _} = ecron:create(
        JobName, "0 10 8,9 * * 1-5", MFA, #{start_time => unlimited, end_time => 12}
    ),
    {error, invalid_opts, _} = ecron:create(
        JobName, "0 10 8,9 * * 1-5", MFA, #{singleton => true1}
    ),
    {ok, JobName} = ecron:create(
        JobName, "0 10 8,9 * * 1-5", MFA, #{start_time => {8, 0, 0}, end_time => unlimited}
    ),
    ok = ecron:delete(JobName),
    {ok, JobName} = ecron:create(JobName, "0 10 8,9 * * 1-5", MFA, #{
        start_time => {8, 0, 0}, end_time => {10, 0, 0}
    }),
    ok = ecron:delete(JobName),
    {ok, JobName} = ecron:create(JobName, "0 10 8,9 * * 1-5", MFA, #{
        start_time => unlimited, end_time => {10, 0, 0}
    }),
    ok = ecron:delete(JobName),
    ok.

max_runtime_ms_aborted(_Config) ->
    JobName = check_mail_with_max_runtime_ms,
    MFA = {?MODULE, mail_delay, [200]},
    {ok, JobName} = ecron:create(JobName, "@every 1s", MFA, #{
        max_runtime_ms => 100, register => ?NAME
    }),
    {ok, Result} = ecron:statistic(?NAME, JobName),
    ?assertMatch(
        #{
            crashed := 0,
            mfa := MFA,
            name := JobName,
            next := _,
            opts := [{singleton, false}, {max_count, unlimited}, {max_runtime_ms, 100}],
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
            crashed := 0,
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
    {ok, UnlimitedJob} = ecron:create(UnlimitedJob, "@every 1s", MFA, #{
        max_runtime_ms => unlimited, register => ?NAME
    }),
    {ok, Result} = ecron:statistic(?NAME, UnlimitedJob),
    ?assertMatch(
        #{
            crashed := 0,
            mfa := MFA,
            name := UnlimitedJob,
            next := _,
            opts := [{singleton, false}, {max_count, unlimited}, {max_runtime_ms, unlimited}],
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
            crashed := 0
        },
        Result2
    ),
    ok = ecron:delete(?NAME, UnlimitedJob),
    Job300 = check_mail_with_300_runtime_ms,
    {ok, Job300} = ecron:create(
        Job300,
        "@every 1s",
        MFA,
        #{max_runtime_ms => 300, register => ?NAME}
    ),
    timer:sleep(1250),
    {ok, Result3} = ecron:statistic(?NAME, Job300),
    ?assertMatch(
        #{
            ok := 1,
            aborted := 0,
            crashed := 0
        },
        Result3
    ),
    ok = ecron:delete(?NAME, Job300),
    ok.
