-module(prop_ecron_predict_datetime).
-include_lib("proper/include/proper.hrl").
-include_lib("ecron/include/ecron.hrl").

-export([prop_predict_cron_datetime/0, prop_predict_cron_datetime/1]).
-export([prop_predict_every_datetime/0, prop_predict_every_datetime/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

prop_predict_cron_datetime(doc) -> "predict cron datetime failed";
prop_predict_cron_datetime(opts) -> [{numtests, 5000}].
prop_predict_cron_datetime() ->
    ?FORALL(Spec, {prop_ecron_spec:extend_spec(), shift(), shift()},
        ?IMPLIES(prop_ecron:check_day_of_month(element(1, Spec)),
            begin
                {CrontabSpec, StartShift, EndShift} = Spec,
                SpecStr = prop_ecron:spec_to_str(CrontabSpec),
                {ok, cron, NewCrontabSpec} = ecron_spec:parse_spec(SpecStr),
                Now = erlang:system_time(millisecond),
                StartTime = shift_time(Now, StartShift),
                EndTime = shift_time(Now, EndShift),
                NewSpec = #{
                    type => cron,
                    crontab => NewCrontabSpec,
                    start_time => StartTime,
                    end_time => EndTime
                },
                Start = ecron:datetime_to_millisecond(local, StartTime),
                End = ecron:datetime_to_millisecond(local, EndTime),
                List = ecron:predict_datetime(activate, NewSpec, Start, End, 500, local, Now),
                NowDateTime = calendar:system_time_to_local_time(Now, millisecond),
                ExpectList = predict_cron_datetime(StartTime, EndTime, NewCrontabSpec, {local, NowDateTime}, 500, []),
                ?WHENFAIL(
                    io:format("Predict ~p\n ~p\nFailed: ~p~n~p~n", [SpecStr, NewSpec, StartTime,
                        {activate, NewSpec, Start, End, 500, local}]),
                    ExpectList =:= List andalso prop_ecron:check_cron_result(NewCrontabSpec, Start, End, List)
                )
            end)
    ).

prop_predict_every_datetime(doc) -> "predict every datetime failed";
prop_predict_every_datetime(opts) -> [{numtests, 3000}].
prop_predict_every_datetime() ->
    ?FORALL(Spec, {range(1, ?MAX_TIMEOUT), shift(), shift()},
        begin
            Now = erlang:system_time(millisecond),
            {Second, StartShift, EndShift} = Spec,
            StartTime = shift_time(Now, StartShift),
            EndTime = shift_time(Now, EndShift),
            NewSpec = #{
                type => every,
                crontab => Second,
                start_time => StartTime,
                end_time => EndTime
            },
            Start = shift_ms(Now, StartShift),
            End = shift_ms(Now, EndShift),
            List = ecron:predict_datetime(activate, NewSpec, Start, End, 10, utc, Now),
            ?WHENFAIL(
                io:format("Predict Failed: ~p ~p~n", [NewSpec, List]),
                prop_ecron:check_every_result(Second,
                    shift_second(Now div 1000, StartShift),
                    shift_second(Now div 1000, EndShift), List)
            )
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

predict_cron_datetime(_Start, _End, _Job, _Now, 0, Acc) -> lists:reverse(Acc);
predict_cron_datetime(Start, End, Job, {TimeZone, Now}, Num, Acc) ->
    Next = next_schedule_datetime(Job, Now),
    case in_range(Next, Start, End) of
        already_ended -> lists:reverse(Acc);
        deactivate -> predict_cron_datetime(Start, End, Job, {TimeZone, Start}, Num, Acc);
        running ->
            predict_cron_datetime(Start, End, Job, {TimeZone, Next},
                Num - 1, [to_rfc3339(TimeZone, Next) | Acc])
    end.
next_schedule_datetime(DateSpec, DateTime) ->
    ForwardDateTime = forward_sec(DateTime),
    next_schedule_datetime(forward, DateSpec, ForwardDateTime).

next_schedule_datetime(done, _, DateTime) -> DateTime;
next_schedule_datetime(forward, DateSpec, DateTime) ->
    #{
        second := SecondSpec, minute := MinuteSpec, hour := HourSpec,
        day_of_month := DayOfMonthSpec, month := MonthSpec,
        day_of_week := DayOfWeekSpec} = DateSpec,
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    {Done, NewDateTime} =
        case valid_datetime(MonthSpec, Month) of
            false when Month =:= 12 ->
                {forward, {{Year + 1, 1, 1}, {0, 0, 0}}};
            false ->
                {forward, {{Year, Month + 1, 1}, {0, 0, 0}}};
            true ->
                DayOfWeek = case calendar:day_of_the_week(Year, Month, Day) of 7 -> 0; DOW -> DOW end,
                DOMValid = valid_datetime(DayOfMonthSpec, Day),
                DOWValid = valid_datetime(DayOfWeekSpec, DayOfWeek),
                case (DOMValid andalso DOWValid) orelse
                    ((DayOfMonthSpec =/= '*') andalso
                        (DayOfWeekSpec =/= '*') andalso
                        (DOMValid orelse DOWValid))
                of
                    false -> {forward, forward_day(DateTime)};
                    true ->
                        case valid_datetime(HourSpec, Hour) of
                            false -> {forward, forward_hour(DateTime)};
                            true ->
                                case valid_datetime(MinuteSpec, Minute) of
                                    false -> {forward, forward_min(DateTime)};
                                    true ->
                                        case valid_datetime(SecondSpec, Second) of
                                            false -> {forward, forward_sec(DateTime)};
                                            true -> {done, DateTime}
                                        end
                                end
                        end
                end
        end,
    next_schedule_datetime(Done, DateSpec, NewDateTime).

forward_sec(DateTime) ->
    FSeconds = calendar:datetime_to_gregorian_seconds(DateTime) + 1,
    calendar:gregorian_seconds_to_datetime(FSeconds).

forward_min(DateTime) ->
    FSeconds = calendar:datetime_to_gregorian_seconds(DateTime) + 60,
    {{Y, Mo, D}, {H, M, _}} = calendar:gregorian_seconds_to_datetime(FSeconds),
    {{Y, Mo, D}, {H, M, 0}}.

forward_hour(DateTime) ->
    FSeconds = calendar:datetime_to_gregorian_seconds(DateTime) + 60 * 60,
    {{Y, Mo, D}, {H, _, _}} = calendar:gregorian_seconds_to_datetime(FSeconds),
    {{Y, Mo, D}, {H, 0, 0}}.

forward_day(DateTime) ->
    FSeconds = calendar:datetime_to_gregorian_seconds(DateTime) + 60 * 60 * 24,
    {{Y, M, D}, _} = calendar:gregorian_seconds_to_datetime(FSeconds),
    {{Y, M, D}, {0, 0, 0}}.

shift_time(_, unlimited) -> unlimited;
shift_time(Current, Ms) -> calendar:system_time_to_local_time(Current + Ms * 1000, millisecond).

shift_ms(_, unlimited) -> unlimited;
shift_ms(Current, Ms) -> Current + Ms * 1000.

shift_second(_, unlimited) -> unlimited;
shift_second(Current, Ms) -> Current + Ms.

-define(AlreadyEndedStatus, already_ended).
-define(WaitingStatus, waiting).
-define(RunningStatus, running).
-define(DeactivateStatus, deactivate).
-define(ActivateStatus, activate).

in_range(_Current, unlimited, unlimited) -> ?RunningStatus;
in_range(Current, unlimited, End) ->
    case second_diff(End, Current) > 0 of
        true -> ?AlreadyEndedStatus;
        false -> ?RunningStatus
    end;
in_range(Current, Start, unlimited) ->
    case second_diff(Start, Current) < 0 of
        true -> ?DeactivateStatus;
        false -> ?RunningStatus
    end;
in_range(Current, Start, End) ->
    case second_diff(End, Current) > 0 of
        true -> ?AlreadyEndedStatus;
        false ->
            case second_diff(Start, Current) < 0 of
                true -> ?DeactivateStatus;
                false -> ?RunningStatus
            end
    end.

second_diff(CurrentDateTime, NextDateTime) ->
    CurrentSeconds = calendar:datetime_to_gregorian_seconds(CurrentDateTime),
    NextSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
    NextSeconds - CurrentSeconds.

valid_datetime('*', _Value) -> true;
valid_datetime([], _Value) -> false;
valid_datetime([Value | _T], Value) -> true;
valid_datetime([{Lower, Upper} | _], Value) when Lower =< Value andalso Value =< Upper -> true;
valid_datetime([_ | T], Value) -> valid_datetime(T, Value).


-define(SECONDS_FROM_0_TO_1970, 719528 * 86400).

to_rfc3339(_TimeZone, unlimited) -> unlimited;
to_rfc3339(TimeZone, Time) ->
    UniversalTime =
        case TimeZone of
            utc -> Time;
            local -> [T1 | _] = calendar:local_time_to_universal_time_dst(Time), T1
        end,
    SystemTime = calendar:datetime_to_gregorian_seconds(UniversalTime) - ?SECONDS_FROM_0_TO_1970,
    calendar:system_time_to_rfc3339(SystemTime).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
shift() ->
    frequency([
        {9, range(-3600 * 12, 3600 * 12)},
        {3, unlimited}
    ]).

