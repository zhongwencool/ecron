-module(prop_ecron_status).
-include_lib("proper/include/proper.hrl").
-include_lib("ecron/include/ecron.hrl").

-export([prop_cron_apply_ok/0, prop_cron_apply_ok/1]).
-export([prop_cron_apply_error/0, prop_cron_apply_error/1]).
-export([prop_unknown/0, prop_unknown/1]).
-export([prop_singleton/0, prop_singleton/1]).
-export([prop_restart_server/0, prop_restart_server/1]).
-export([prop_send_after/0, prop_send_after/1]).
-export([prop_ecron_send_interval/0, prop_ecron_send_interval/1]).
-export([prop_add_with_count/0, prop_add_with_count/1]).

-export([echo/2]).
-export([long_echo/3]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_cron_apply_ok(doc) -> "add cron failed";
prop_cron_apply_ok(opts) -> [{numtests, 20}].
prop_cron_apply_ok() ->
    ?FORALL(
        {Name, Request, FuncType},
        {name(), term(), oneof([mfa, func, wrong])},
        begin
            error_logger:tty(false),
            application:set_env(ecron, local_jobs, [
                {good_yearly, "@yearly", {io, format, ["Yearly~n"]}}
            ]),
            application:set_env(ecron, adjusting_time_second, 1),
            application:ensure_all_started(ecron),
            MFA =
                case FuncType of
                    mfa -> {?MODULE, echo, [self(), Request]};
                    func -> {fun echo/2, [self(), Request]};
                    wrong -> {?MODULE, echo, [undefined, Request]}
                end,
            {ok, Name} = ecron:add(Name, "*/2 * * * * *", MFA),
            case FuncType =/= wrong of
                true ->
                    Res = check_normal_response(Request, 2, 2),
                    ok = ecron:delete(Name),
                    application:set_env(ecron, adjusting_time_second, 100000),
                    error_logger:tty(true),
                    Res;
                false ->
                    Res = check_normal_response(Request, 2, 2),
                    application:set_env(ecron, adjusting_time_second, 100000),
                    error_logger:tty(true),
                    ok =:= ecron:delete(Name) andalso (not Res)
            end
        end
    ).

prop_cron_apply_error(doc) -> "add cron failed";
prop_cron_apply_error(opts) -> [{numtests, 10}].
prop_cron_apply_error() ->
    ?FORALL(
        {Name, Request},
        {name(), term()},
        begin
            application:set_env(ecron, local_jobs, []),
            application:ensure_all_started(ecron),
            error_logger:tty(false),
            OkMFA = {?MODULE, long_echo, [650, self(), ?FUNCTION_NAME]},
            WrongMFA = {?MODULE, echo, [wrong, Request]},
            {ok, OkName} = ecron:add({Name, ?FUNCTION_NAME}, "@every 1s", OkMFA),
            {ok, WrongName} = ecron:add(Name, "@every 1s", WrongMFA),
            timer:sleep(1900),
            {ok, #{
                crontab := CronSpec,
                start_time := {0, 0, 0},
                end_time := {23, 59, 59},
                mfa := RMFA,
                name := RName,
                failed := Failed,
                ok := Ok,
                results := Result
            }} = ecron:statistic(ecron_local, WrongName),
            {ok, #{
                failed := Failed1,
                ok := Ok1
            }} = ecron:statistic(ecron_local, OkName),
            ok = ecron:delete(WrongName),
            ok = ecron:delete(OkName),
            error_logger:tty(true),
            CronSpec =:= 1 andalso
                RName =:= WrongName andalso
                WrongMFA =:= RMFA andalso
                Ok =:= 0 andalso
                Ok1 =:= 1 andalso
                Failed =:= length(Result) andalso
                Failed1 =:= 0 andalso
                lists:all(
                    fun(R) -> {element(1, R), element(2, R)} =:= {error, function_clause} end,
                    Result
                )
        end
    ).

prop_unknown(doc) -> "unknown message";
prop_unknown(opts) -> [{numtests, 10}].
prop_unknown() ->
    ?FORALL(
        Message,
        term(),
        begin
            application:ensure_all_started(ecron),
            Pid = erlang:whereis(?LocalJob),
            CallRes = (catch gen_server:call(Pid, Message, 100)),
            gen_server:cast(Pid, Message),
            erlang:send(Pid, Message),
            NewPid = erlang:whereis(?LocalJob),
            Pid =:= NewPid andalso
                {'EXIT', {timeout, {gen_server, call, [Pid, Message, 100]}}} =:= CallRes
        end
    ).

prop_restart_server(doc) -> "restart server";
prop_restart_server(opts) -> [{numtests, 10}].
prop_restart_server() ->
    ?FORALL(
        Name,
        term(),
        begin
            error_logger:tty(false),
            application:set_env(ecron, local_jobs, [
                {ecron_test_1, "@yearly", {io, format, ["Yearly~n"]}},
                {ecron_test_2, "@yearly", {io, format, ["Yearly~n"]}, unlimited, unlimited}
            ]),
            application:ensure_all_started(ecron),
            {ok, Name} = ecron:add(Name, "@yearly", {io, format, ["Yearly~n"]}),
            Res1 = ecron:statistic(ecron_local, Name),
            Pid = erlang:whereis(?LocalJob),
            erlang:exit(Pid, kill),
            timer:sleep(200),
            NewPid = erlang:whereis(?LocalJob),
            Res2 = ecron:statistic(ecron_local, Name),
            ok = ecron:delete(Name),
            error_logger:tty(true),
            Pid =/= NewPid andalso
                element(1, Res1) =:= ok andalso
                element(1, Res2) =:= ok
        end
    ).

prop_singleton(doc) -> "singleton";
prop_singleton(opts) -> [{numtests, 10}].
prop_singleton() ->
    ?FORALL(
        {Name, Singleton},
        {term(), bool()},
        begin
            error_logger:tty(false),
            application:start(telemetry),
            application:set_env(ecron, local_jobs, [
                {crontab_job_xyz, "@yearly", {io, format, ["Yearly~n"]}, unlimited, unlimited, [
                    {singleton, Singleton}
                ]}
            ]),
            application:set_env(ecron, adjusting_time_second, 1),
            application:stop(ecron),
            application:start(ecron),
            {ok, Name} = ecron:add(
                Name, "@every 1s", {timer, sleep, [1100]}, unlimited, unlimited, [
                    {singleton, Singleton}
                ]
            ),
            timer:sleep(4200),
            {ok, Res} = ecron:statistic(ecron_local, Name),
            #{
                start_time := {0, 0, 0},
                end_time := {23, 59, 59},
                status := activate,
                failed := 0,
                ok := Ok,
                results := Results,
                run_microsecond := RunMs
            } = Res,
            ecron:delete(Name),
            Num =
                case Singleton of
                    true -> 2;
                    false -> 3
                end,
            application:set_env(ecron, adjusting_time_second, 100000),
            error_logger:tty(true),
            Ok =:= Num andalso length(Results) =:= Num andalso length(RunMs) =:= Num
        end
    ).

prop_send_after(doc) -> "send_after";
prop_send_after(opts) -> [{numtests, 10}].
prop_send_after() ->
    ?FORALL(
        Message,
        term(),
        begin
            {ok, _} = ecron:send_after("@every 2s", self(), Message),
            Res =
                receive
                    Message -> ok
                after 2100 -> error
                end,
            {ok, Ref} = ecron:send_after("@every 1s", self(), Message),
            {error, invalid_spec, "@every1 1s"} = ecron:send_after("@every1 1s", self(), Message),
            RMS = erlang:cancel_timer(Ref),
            Res2 =
                receive
                    Message -> ok
                after 1100 -> error
                end,
            Res =:= ok andalso Res2 =:= error andalso RMS =< 1000
        end
    ).

prop_ecron_send_interval(doc) -> "send_interval";
prop_ecron_send_interval(opts) -> [{numtests, 10}].
prop_ecron_send_interval() ->
    ?FORALL(
        {Message, NeedReg, Name},
        {term(), bool(), atom()},
        begin
            error_logger:tty(false),
            application:ensure_all_started(ecron),
            Target =
                case NeedReg of
                    false ->
                        spawn(fun store/0);
                    true ->
                        Pid = spawn(fun store/0),
                        true = erlang:register(Name, Pid),
                        Name
                end,
            {ok, Job} = ecron:send_interval("* * * * * *", Target, {add, self(), Message}),
            Res1 =
                receive
                    Message -> ok
                after 1100 -> error
                end,
            Res2 =
                receive
                    Message -> ok
                after 1100 -> error
                end,
            Res3 =
                receive
                    Message -> ok
                after 1100 -> error
                end,
            {ok, Res} = ecron:statistic(ecron_local, Job),
            #{
                start_time := {0, 0, 0},
                end_time := {23, 59, 59},
                status := activate,
                failed := 0,
                ok := Ok,
                results := Results,
                run_microsecond := RunMs
            } = Res,
            erlang:send(Target, {exit, self()}),
            Res4 =
                receive
                    exit -> ok
                after 800 -> error
                end,
            timer:sleep(160),
            {error, not_found} = ecron:statistic(ecron_local, Job),
            {ok, Job1} = ecron:send_interval(
                ecron_local, make_ref(), "0 1 1 * * *", Message, unlimited, unlimited, []
            ),
            error_logger:tty(true),
            Res1 =:= Res2 andalso Res2 =:= Res3 andalso Res1 =:= ok andalso Res4 =:= ok andalso
                length(Results) =:= Ok andalso length(RunMs) =:= Ok andalso Job1 =/= Job
        end
    ).

prop_add_with_count(doc) -> "add_with_count";
prop_add_with_count(opts) -> [{numtests, 5}].
prop_add_with_count() ->
    ?FORALL(
        _Name,
        term(),
        begin
            application:ensure_all_started(ecron),
            {ok, _Name1} = ecron:add_with_count("@every 1s", {erlang, send, [self(), test]}, 2),
            Res1 =
                receive
                    test -> ok
                after 1100 -> error
                end,
            Res2 =
                receive
                    test -> ok
                after 1100 -> error
                end,
            Res3 =
                receive
                    test -> ok
                after 1100 -> error
                end,
            Res1 =:= ok andalso Res2 =:= ok andalso Res3 =:= error
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
echo(Pid, Msg) when is_pid(Pid) ->
    erlang:send(Pid, {now_millisecond(), Msg}).

long_echo(MilliSec, Pid, Msg) ->
    timer:sleep(MilliSec),
    echo(Pid, Msg).

check_normal_response(Msg, Sec, Number) ->
    check_normal_response(Msg, Number, Sec * 1000 + 60, now_millisecond()).

check_normal_response(_Msg, 0, _Ms, _) ->
    true;
check_normal_response(Msg, Number, Ms, LastMs) ->
    receive
        {NowMs, Msg} ->
            case NowMs - LastMs < Ms of
                true -> check_normal_response(Msg, Number - 1, Ms, NowMs);
                false -> false
            end
    after Ms ->
        false
    end.

now_millisecond() -> erlang:system_time(millisecond).

store() ->
    receive
        {exit, Pid} ->
            erlang:send(Pid, exit);
        {add, Pid, Message} ->
            erlang:send(Pid, Message),
            store()
    after 1100 -> ok
    end.

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
name() -> term().
