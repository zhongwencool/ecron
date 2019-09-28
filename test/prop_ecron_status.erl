-module(prop_ecron_status).
-include_lib("proper/include/proper.hrl").
-include_lib("ecron/include/ecron.hrl").

-export([prop_cron_apply_ok/0, prop_cron_apply_ok/1]).
-export([prop_cron_apply_error/0, prop_cron_apply_error/1]).
-export([prop_already_end/0, prop_already_end/1]).
-export([prop_deactivate/0, prop_deactivate/1]).
-export([prop_unknown/0, prop_unknown/1]).
-export([prop_singleton/0, prop_singleton/1]).
-export([prop_auto_remove/0, prop_auto_remove/1]).
-export([prop_deactivate_already_ended/0, prop_deactivate_already_ended/1]).
-export([prop_restart_server/0, prop_restart_server/1]).

-export([echo/2]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_cron_apply_ok(doc) -> "add cron failed";
prop_cron_apply_ok(opts) -> [{numtests, 20}].
prop_cron_apply_ok() ->
    ?FORALL({Name, Request, FuncType}, {name(), term(), oneof([mfa, func, wrong])},
        begin
            error_logger:tty(false),
            application:set_env(ecron, jobs, [{good_yearly, "@yearly", {io, format, ["Yearly~n"]}}]),
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
        end).

prop_cron_apply_error(doc) -> "add cron failed";
prop_cron_apply_error(opts) -> [{numtests, 10}].
prop_cron_apply_error() ->
    ?FORALL({Name, Request}, {name(), term()},
        begin
            application:set_env(ecron, jobs, []),
            application:ensure_all_started(ecron),
            error_logger:tty(false),
            MFA = {?MODULE, echo, [wrong, Request]},
            {ok, Name} = ecron:add(Name, "@every 1s", MFA),
            timer:sleep(1100),
            {ok, #{crontab := CronSpec,
                end_time := unlimited, start_time := unlimited,
                mfa := RMFA, name := RName,
                failed := Failed, ok := Ok,
                results := Result}} = ecron:statistic(Name),
            ok = ecron:delete(Name),
            error_logger:tty(true),
            CronSpec =:= 1 andalso
                RName =:= Name andalso
                MFA =:= RMFA andalso
                Ok =:= 0 andalso
                Failed =:= length(Result) andalso
                lists:all(fun(R) -> R =:= {error, function_clause} end, Result)
        end).

prop_already_end(doc) -> "check already_end failed";
prop_already_end(opts) -> [{numtests, 300}].
prop_already_end() ->
    ?FORALL({Name, Request, Shift}, {name(), term(), range(100, 1000)},
        begin
            application:set_env(ecron, jobs, []),
            application:ensure_all_started(ecron),
            EndTime = reduce_time(Shift),
            Name1 = {1, Name},
            Err1 = ecron:add(Name, "* * * * * *", {?MODULE, echo, [self(), Request]}, unlimited, EndTime),
            Err2 = ecron:add(Name1, "@every 1s", {?MODULE, echo, [self(), Request]}, unlimited, EndTime),
            Err3 = ecron:statistic(Name),
            Err4 = ecron:statistic(Name1),
            Err1 =:= {error, already_ended} andalso
                Err2 =:= {error, already_ended} andalso
                Err3 =:= {error, not_found} andalso
                Err4 =:= {error, not_found}
        end).

prop_deactivate(doc) -> "deactive failed";
prop_deactivate(opts) -> [{numtests, 10}].
prop_deactivate() ->
    ?FORALL({Name, Request, Shift}, {name(), term(), range(3, 4)},
        begin
            application:set_env(ecron, jobs, []),
            application:set_env(ecron, adjusting_time_second, 1),
            application:ensure_all_started(ecron),
            StartTime = add_time(Shift),
            {ok, Name} = ecron:add(Name, "* * * * * *", {fun echo/2, [self(), Request]}, StartTime, unlimited),
            {ok, #{status := Activate}} = ecron:statistic(Name),
            ok = ecron:deactivate(Name),
            {ok, #{status := Deactivate}} = ecron:statistic(Name),
            ok = ecron:activate(Name),
            timer:sleep(Shift * 1000 + 60),
            Res = check_normal_response(Request, Shift, 2),
            ok = ecron:delete(Name),
            {error, not_found} = ecron:statistic(Name),
            application:set_env(ecron, adjusting_time_second, 100000),
            Deactivate =:= deactivate andalso Activate =:= activate andalso Res
        end).

prop_unknown(doc) -> "unknown message";
prop_unknown(opts) -> [{numtests, 10}].
prop_unknown() ->
    ?FORALL(Message, term(),
        begin
            application:ensure_all_started(ecron),
            Pid = erlang:whereis(?Ecron),
            CallRes = (catch gen_server:call(Pid, Message, 100)),
            gen_server:cast(Pid, Message),
            erlang:send(Pid, Message),
            NewPid = erlang:whereis(?Ecron),
            Pid =:= NewPid andalso
                {'EXIT', {timeout, {gen_server, call, [Pid, Message, 100]}}} =:= CallRes
        end).

prop_restart_server(doc) -> "restart server";
prop_restart_server(opts) -> [{numtests, 10}].
prop_restart_server() ->
    ?FORALL(Name, term(),
        begin
            error_logger:tty(false),
            application:set_env(ecron, jobs, [
                {ecron_test_1, "@yearly", {io, format, ["Yearly~n"]}},
                {ecron_test_2, "@yearly", {io, format, ["Yearly~n"]}, unlimited, unlimited}
            ]),
            application:ensure_all_started(ecron),
            {ok, Name} = ecron:add(Name, "@yearly", {io, format, ["Yearly~n"]}),
            Res1 = ecron:statistic(Name),
            Pid = erlang:whereis(?Ecron),
            erlang:exit(Pid, killed),
            timer:sleep(200),
            NewPid = erlang:whereis(?Ecron),
            Res2 = ecron:statistic(Name),
            ok = ecron:delete(Name),
            error_logger:tty(true),
            Pid =/= NewPid andalso
                element(1, Res1) =:= ok andalso
                element(1, Res2) =:= ok
        end).

prop_singleton(doc) -> "singleton";
prop_singleton(opts) -> [{numtests, 10}].
prop_singleton() ->
    ?FORALL({Name, Singleton}, {term(), bool()},
        begin
            error_logger:tty(false),
            application:set_env(ecron, jobs, [{crontab_job_xyz, "@yearly", {io, format, ["Yearly~n"]}, unlimited, unlimited, [{singleton, Singleton}]}]),
            application:set_env(ecron, adjusting_time_second, 1),
            application:stop(ecron),
            application:start(ecron),
            {ok, Name} = ecron:add(Name, "@every 1s", {timer, sleep, [1100]}, unlimited, unlimited, [{singleton, Singleton}]),
            timer:sleep(4200),
            {ok, Res} = ecron_tick:statistic(Name),
            #{start_time := unlimited, end_time := unlimited, status := activate,
                failed := 0, ok := Ok, results := Results, run_microsecond := RunMs
            } = Res,
            ecron:delete(Name),
            Num = case Singleton of true -> 2; false -> 3 end,
            application:set_env(ecron, adjusting_time_second, 100000),
            error_logger:tty(true),
            Ok =:= Num andalso length(Results) =:= Num andalso length(RunMs) =:= Num
        end).

prop_auto_remove(doc) -> "auto remove after already_ended";
prop_auto_remove(opts) -> [{numtests, 5}].
prop_auto_remove() ->
    ?FORALL(Name, term(),
        begin
            application:ensure_all_started(ecron),
            Shift = 2000,
            EndMs = erlang:system_time(millisecond) + Shift,
            EndTime = calendar:system_time_to_local_time(EndMs, millisecond),
            {ok, Name} = ecron:add(Name, "@every 1s", {timer, sleep, [500]}, unlimited, EndTime),
            timer:sleep(Shift + 100),
            Result = ecron_tick:statistic(Name),
            Result =:= {error, not_found}
        end).

prop_deactivate_already_ended(doc) -> "auto remove after already_ended";
prop_deactivate_already_ended(opts) -> [{numtests, 5}].
prop_deactivate_already_ended() ->
    ?FORALL(Name, term(),
        begin
            application:ensure_all_started(ecron),
            Shift = 2000,
            Now = erlang:system_time(millisecond),
            EndMs = Now + Shift,
            EndTime = calendar:system_time_to_local_time(EndMs, millisecond),
            StartMs = Now - Shift,
            StartTime = calendar:system_time_to_local_time(StartMs, millisecond),
            {error, invalid_time, {EndTime, StartTime}} = ecron:add("@every 1s", {timer, sleep, [500]}, EndTime, StartTime),
            {ok, Name} = ecron:add(Name, "@every 1s", {timer, sleep, [500]}, unlimited, EndTime),
            ok = ecron:deactivate(Name),
            timer:sleep(Shift + 100),
            Result = ecron:activate(Name),
            Result =:= {error, already_ended}
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
echo(Pid, Msg) when is_pid(Pid) ->
    erlang:send(Pid, {now_millisecond(), Msg}).

check_normal_response(Msg, Sec, Number) ->
    check_normal_response(Msg, Number, Sec * 1000 + 60, now_millisecond()).

check_normal_response(_Msg, 0, _Ms, _) -> true;
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

reduce_time(Shift) ->
    Now = calendar:local_time(),
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) - Shift).

add_time(Shift) ->
    Now = calendar:local_time(),
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + Shift).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
name() -> term().
