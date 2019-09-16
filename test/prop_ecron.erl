-module(prop_ecron).
-include_lib("proper/include/proper.hrl").

-export([prop_cron_apply_ok/0, prop_cron_apply_ok/1]).
-export([prop_cron_apply_error/0, prop_cron_apply_error/1]).
-export([prop_already_end/0, prop_already_end/1]).
-export([prop_deactivate/0, prop_deactivate/1]).

-export([echo/2]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_cron_apply_ok(doc) -> "add cron failed";
prop_cron_apply_ok(opts) -> [{numtests, 10}].
prop_cron_apply_ok() ->
    ?FORALL({Name, Request}, {name(), term()},
        begin
            application:set_env(ecron, jobs, []),
            application:set_env(ecron, adjusting_time_second, 1),
            application:ensure_all_started(ecron),
            {ok, Pid} = ecron:add(Name, "*/2 * * * * *", {fun echo/2, [self(), Request]}),
            UnKnowRes = gen_server:call(Pid, unknow_call),
            Result = check_normal_response(Request, 2, 2),
            ok = ecron:delete(Name),
            application:set_env(ecron, adjusting_time_second, 100000),
            Result andalso UnKnowRes =:= error
        end).

prop_cron_apply_error(doc) -> "add cron failed";
prop_cron_apply_error(opts) -> [{numtests, 5}].
prop_cron_apply_error() ->
    ?FORALL({Name, Request}, {name(), term()},
        begin
            application:set_env(ecron, jobs, []),
            application:ensure_all_started(ecron),
            error_logger:tty(false),
            MFA = {?MODULE, echo, [wrong, Request]},
            {ok, Pid} = ecron:add(Name, "@every 1s", MFA),
            erlang:send(Pid, unknow_msg),
            timer:sleep(1100),
            {ok, #{ecron :=
            #{crontab := CronSpec, end_time := unlimited, start_time := unlimited, mfa := RMFA, name := RName},
                failed := Failed, ok := Ok,
                results := Result}
            } = ecron:statistic(Name),
            ok = ecron:delete(Name),
            error_logger:tty(true),
            CronSpec =:= 1000 andalso
                RName =:= Name andalso
                MFA =:= RMFA andalso
                Ok =:= 0 andalso
                Failed =:= length(Result) andalso
                lists:all(fun(R) -> R =:= {error, function_clause} end, Result)
        end).

prop_already_end(doc) -> "check already_end failed";
prop_already_end(opts) -> [{numtests, 100}].
prop_already_end() ->
    ?FORALL({Name, Request, Shift}, {name(), term(), range(100, 1000)},
        begin
            application:set_env(ecron, jobs, []),
            application:ensure_all_started(ecron),
            EndTime = reduce_time(Shift),
            Name1 = {1, Name},
            {ok, _Pid} = ecron:add(Name, "* * * * * *", {?MODULE, echo, [self(), Request]}, unlimited, EndTime),
            {ok, _Pid1} = ecron:add(Name1, "@every 1s", {?MODULE, echo, [self(), Request]}, unlimited, EndTime),
            timer:sleep(150),
            {ok, #{status := Status}} = ecron:statistic(Name),
            {ok, #{status := Status1}} = ecron:statistic(Name1),
            ok = ecron:delete(Name),
            ok = ecron:delete(Name1),
            Status =:= already_ended andalso Status1 =:= already_ended
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
            {ok, _Pid} = ecron:add(Name, "* * * * * *", {fun echo/2, [self(), Request]}, StartTime, unlimited),
            {ok, #{status := Deactivate}} = ecron:statistic(Name),
            timer:sleep(Shift * 1000 + 70),
            {ok, #{status := Activate}} = ecron:statistic(Name),
            Res = check_normal_response(Request, Shift, 2),
            ok = ecron:delete(Name),
            application:set_env(ecron, adjusting_time_second, 100000),
            Deactivate =:= deactivate andalso Activate =/= deactivate andalso Res
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
echo(Pid, Msg) when is_pid(Pid) ->
    erlang:send(Pid, {now_millisecond(), Msg}).

check_normal_response(Msg, Sec, Number) ->
    check_normal_response(Msg, Number, Sec * 1000 + 100, now_millisecond()).

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

now_millisecond() -> erlang:convert_time_unit(erlang:system_time(), native, millisecond).

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
