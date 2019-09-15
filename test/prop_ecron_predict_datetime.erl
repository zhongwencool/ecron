-module(prop_ecron_predict_datetime).
-include_lib("proper/include/proper.hrl").

-export([prop_predict_every_datetime/0, prop_predict_every_datetime/1]).
-export([prop_predict_cron_datetime/0, prop_predict_cron_datetime/1]).
-export([check_cron_result/5, check_every_result/6]).

-import(prop_ecron_helper, [spec/0, extend_spec/0, spec_to_str/1, unzip/1, check_day_of_month/1, to_now_datetime/2]).

-define(MAX_TIMEOUT, 4294967). %% (16#ffffffff div 1000) 49.71 days.
-define(unlimited, unlimited).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_predict_every_datetime(doc) -> "predict every datetime failed";
prop_predict_every_datetime(opts) -> [{numtests, 3000}].
prop_predict_every_datetime() ->
    ?FORALL(Spec, {range(1, ?MAX_TIMEOUT), shift(), shift()},
        begin
            Now = calendar:local_time(),
            {Second, StartShift, EndShift} = Spec,
            StartTime = shift_time(Now, StartShift),
            EndTime = shift_time(Now, EndShift),
            NewSpec = #{
                type => every,
                crontab => Second * 1000,
                start_time => StartTime,
                end_time => EndTime
            },
            ActualTime = case is_greater_than_or_equal(Now, StartTime) of true -> Now; false -> StartTime end,
            ExpectList = ecron_job:predict_datetime(NewSpec, {local, ActualTime}, 500),
            ?WHENFAIL(
                io:format("Predict ~p Failed: ~p~n", [NewSpec, ActualTime]),
                check_every_result(Second, local, StartTime, EndTime, ActualTime, ExpectList)
            )
        end).

prop_predict_cron_datetime(doc) -> "predict cron datetime failed";
prop_predict_cron_datetime(opts) -> [{numtests, 4000}].
prop_predict_cron_datetime() ->
    ?FORALL(Spec, {extend_spec(), shift(), shift()},
        ?IMPLIES(check_day_of_month(element(1, Spec)),
            begin
                {CrontabSpec, StartShift, EndShift} = Spec,
                SpecStr = spec_to_str(CrontabSpec),
                {ok, cron, NewCrontabSpec} = ecron:parse_spec(SpecStr),
                Now = calendar:universal_time(),
                StartTime = shift_time(Now, StartShift),
                EndTime = shift_time(Now, EndShift),
                NewSpec = #{
                    type => cron,
                    crontab => NewCrontabSpec,
                    start_time => StartTime,
                    end_time => EndTime
                },
                ActualTime = case is_greater_than_or_equal(Now, StartTime) of true -> Now; false -> StartTime end,
                List = ecron_job:predict_datetime(NewSpec, {utc, ActualTime}, 400),
                ?WHENFAIL(
                    io:format("Predict ~p Failed: ~p~n", [NewSpec, ActualTime]),
                    check_cron_result(NewCrontabSpec, utc, StartTime, EndTime, List)
                )
            end)
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
shift_time(_, unlimited) -> unlimited;
shift_time(DateTime, Ms) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(DateTime) + Ms).

check_every_result(_Second, _Type, _Start, _End, _Now, []) -> true;
check_every_result(Second, Type, Start, End, Now, [T1 | Rest]) ->
    T = to_now_datetime(Type, T1),
    Differ = calendar:datetime_to_gregorian_seconds(T)
        - calendar:datetime_to_gregorian_seconds(Now),
    case is_greater_than_or_equal(T, Start) andalso
        is_greater_than_or_equal(End, T) andalso
        (Differ rem Second == 0) of
        true ->check_every_result(Second, Type, Start, End, Now, Rest);
        false -> false
    end.

check_cron_result(_DateSpec, _Type, _StartTime, _EndTime, []) -> true;
check_cron_result(DateSpec, Type, StartTime, EndTime, [T1 | Rest]) ->
    T = to_now_datetime(Type, T1),
    case is_greater_than_or_equal(T, StartTime) andalso
        is_greater_than_or_equal(EndTime, T) andalso
        in_cron_range(DateSpec, T) of
        true -> check_cron_result(DateSpec, Type, StartTime, EndTime, Rest);
        false -> false
    end.

in_cron_range(DateSpec, {{Year, Month, Day}, {Hour, Minute, Second}}) ->
    #{
        second := SecondSpec, minute := MinuteSpec, hour := HourSpec,
        day_of_month := DayOfMonthSpec, month := MonthSpec,
        day_of_week := DayOfWeekSpec
    } = DateSpec,
    Week = case calendar:day_of_the_week(Year, Month, Day) of 7 -> 0; DOW -> DOW end,
    in_cron_range2(Second, SecondSpec) andalso
        in_cron_range2(Minute, MinuteSpec) andalso
        in_cron_range2(Hour, HourSpec) andalso
        in_cron_range2(Month, MonthSpec) andalso
        in_cron_week_month_day_range(Week, Day, DayOfWeekSpec, DayOfMonthSpec).

in_cron_week_month_day_range(Week, Day, DayOfWeekSpec, DayOfMonthSpec) ->
    DOW = in_cron_range2(Week, DayOfWeekSpec),
    DOM = in_cron_range2(Day, DayOfMonthSpec),
    DayOfWeekSpec =/= '*' andalso DayOfMonthSpec =/= '*' andalso (DOW orelse DOM) orelse
        (DOW andalso DOM).

in_cron_range2(_, '*') -> true;
in_cron_range2(Value, ZipValues) -> lists:member(Value, unzip(ZipValues)).

is_greater_than_or_equal(unlimited, _Datetime) -> true;
is_greater_than_or_equal(_, unlimited) -> true;
is_greater_than_or_equal(DateTimeA, DateTimeB) ->
    calendar:datetime_to_gregorian_seconds(DateTimeA) >= calendar:datetime_to_gregorian_seconds(DateTimeB).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
shift() ->
    frequency([
        {9, integer()},
        {1, unlimited}
    ]).
