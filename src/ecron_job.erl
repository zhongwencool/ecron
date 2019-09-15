-module(ecron_job).
-behaviour(gen_server).

-export([next_schedule_datetime/2]).
-export([predict_datetime/3]).

-export([activate/1, deactivate/1]).
-export([statistic/1]).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_SIZE, 20).

-define(Already_ended, already_ended).
-define(Waiting, waiting).
-define(Running, running).
-define(Deactivate, deactivate).
-define(Activate, activate).

-define(Countdown_to_activate, countdown_to_activate).
-define(Stop, stop).
-define(Statistic, statistic).
-define(Reload, reload).

start_link(Ecron) -> gen_server:start_link(?MODULE, [Ecron], []).
stop(Pid) -> gen_server:call(Pid, ?Stop).
activate(Pid) -> gen_server:call(Pid, ?Activate).
deactivate(Pid) -> gen_server:call(Pid, ?Deactivate).
statistic(Pid) -> gen_server:call(Pid, ?Statistic).

init([Ecron]) ->
    #{start_time := StartTime, name := Name} = Ecron,
    TimeType = application:get_env(ecron, time_type, local),
    count_down_to_activate(StartTime, TimeType),
    case ets:insert_new(ecron, {Name, self()}) of
        true ->
            erlang:process_flag(trap_exit, true),
            {ok, #{
                worker => undefined, ecron => Ecron,
                status => ?Deactivate, results => [],
                run_microsecond => [], time_type => TimeType,
                ok => 0, failed => 0}
            };
        false ->
            {stop, {shutdown, already_exist}}
    end.

handle_call(?Activate, _From, State = #{worker := Worker}) ->
    erlang:is_pid(Worker) andalso erlang:exit(Worker, ?Deactivate),
    {reply, ok, do_activate(State#{status =>?Deactivate, worker => undefined})};

handle_call(?Deactivate, _From, State = #{worker := Worker}) ->
    erlang:is_pid(Worker) andalso erlang:exit(Worker, ?Deactivate),
    {reply, ok, State#{status => ?Deactivate, worker => undefined}};

handle_call(?Statistic, _From, State = #{ecron := Ecron, time_type := TimeType}) ->
    Next = predict_datetime(Ecron, {TimeType, current_datetime(TimeType)}, 10),
    #{start_time := StartTime, end_time := EndTime} = Ecron,
    {reply,
        State#{
            next => Next,
            ecron =>
            Ecron#{start_time => to_rfc3339(TimeType, StartTime),
                end_time => to_rfc3339(TimeType, EndTime)}},
        State};

handle_call(?Stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast({update_status, Status, Worker}, State = #{status := OldStatus})
    when OldStatus =/= ?Deactivate andalso OldStatus =/= ?Already_ended ->
    {noreply, State#{status => Status, worker => Worker}};
handle_cast({update_run_statistic, Status, Start, End, {OkInc, FailedInc, Result}},
    State = #{status := OldStatus})
    when OldStatus =/= ?Deactivate andalso OldStatus =/= ?Already_ended ->
    Cost = erlang:convert_time_unit(End - Start, native, microsecond),
    #{ok := OkCount, failed := FailedCount, results := Results, run_microsecond := Costs} = State,
    {noreply, State#{
        status => Status,
        ok => OkCount + OkInc,
        failed => FailedCount + FailedInc,
        run_microsecond => lists:sublist([Cost | Costs], ?MAX_SIZE),
        results => lists:sublist([Result | Results], ?MAX_SIZE)}
    };
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(?Countdown_to_activate,
    State = #{ecron := #{start_time := StartTime}, time_type := TimeType}) ->
    count_down_to_activate(StartTime, TimeType),
    {noreply, State};
handle_info(?Activate, State = #{status := ?Deactivate}) ->
    {noreply, do_activate(State)};
handle_info(?Activate, State) ->
    {noreply, State};
handle_info({'EXIT', _OldWorker, ?Deactivate}, State) ->
    {noreply, State};
handle_info({'EXIT', Worker, normal}, State = #{worker := Worker}) ->
    {noreply, State#{worker => undefined}};
handle_info({'EXIT', _OldWorker, normal}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    logger:warning("~p unknow message:~p~n ~p~n", [self(), Info, State]),
    {noreply, State}.

terminate(_Reason, #{ecron := #{name := JobName}, worker := Worker}) ->
    is_pid(Worker) andalso erlang:exit(Worker, killed),
    ets:delete(ecron, JobName),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

predict_datetime(#{type := every, crontab := Ms, start_time := Start, end_time := End},
    Now, Num) ->
    predict_every_datetime(Start, End, trunc(Ms / 1000), Now, Num, []);
predict_datetime(#{type := cron, crontab := Job, start_time := Start, end_time := End},
    Now, Num) ->
    predict_cron_datetime(Start, End, Job, Now, Num, []);
predict_datetime(Type, Job, Num) ->
    TimeType = application:get_env(ecron, time_type, local),
    Now = current_datetime(TimeType),
    predict_datetime(#{type => Type, crontab => Job, start_time => unlimited, end_time => unlimited},
        {TimeType, Now}, Num).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_activate(State = #{ecron := Ecron, status := ?Deactivate, time_type := TimeType}) ->
    MaxTimeout = application:get_env(ecron, adjusting_time_millisecond, 7 * 24 * 3600),
    #{end_time := EndTime} = Ecron,
    EndSec = case EndTime of unlimited -> unlimited; _ -> calendar:datetime_to_gregorian_seconds(EndTime) end,
    Self = self(),
    Worker = spawn_link(fun() -> schedule_job(Ecron#{end_time := EndSec}, Self, MaxTimeout, TimeType) end),
    State#{worker => Worker, status => ?Waiting}.

schedule_job(#{type := every, crontab := Ms, mfa := MFA, end_time := EndSec} = EcronSpec,
    ParentPid, MaxTimeout, TimeType) ->
    case unlimited =:= EndSec orelse
        calendar:datetime_to_gregorian_seconds(current_datetime(TimeType)) < EndSec
    of
        true ->
            gen_server:cast(ParentPid, {update_status, ?Running, self()}),
            Start = erlang:monotonic_time(),
            Result = safe_apply_job(MFA),
            gen_server:cast(ParentPid, {update_run_statistic, ?Waiting, Start, erlang:monotonic_time(), Result}),
            timer:sleep(Ms),
            schedule_job(EcronSpec, ParentPid, MaxTimeout, TimeType);
        false ->
            gen_server:cast(ParentPid, {update_status, ?Already_ended, undefined})
    end;
schedule_job(#{type := cron, crontab := DateSpec, mfa := MFA, end_time := EndSec} = EcronSpec,
    ParentPid, MaxTimeout, TimeType) ->
    CurrentDateTime = current_datetime(TimeType),
    case unlimited =:= EndSec orelse
        calendar:datetime_to_gregorian_seconds(CurrentDateTime) < EndSec
    of
        true ->
            NextValidDateTime = next_schedule_datetime(DateSpec, CurrentDateTime),
            SleepMs = time_diff_ms(CurrentDateTime, NextValidDateTime),
            gen_server:cast(ParentPid, {update_status, ?Waiting, self()}),
            case SleepMs > MaxTimeout of
                true ->
                    timer:sleep(MaxTimeout),
                    schedule_job(EcronSpec, ParentPid, MaxTimeout, TimeType);
                false ->
                    timer:sleep(SleepMs),
                    gen_server:cast(ParentPid, {update_status, ?Running, self()}),
                    Start = erlang:monotonic_time(),
                    Res = safe_apply_job(MFA),
                    gen_server:cast(ParentPid,
                        {update_run_statistic, ?Waiting, Start, erlang:monotonic_time(), Res}),
                    schedule_job(EcronSpec, ParentPid, MaxTimeout, TimeType)
            end;
        false ->
            gen_server:cast(ParentPid, {update_status, ?Already_ended, undefined})
    end.

safe_apply_job(Exec) ->
    try
        case Exec of
            {M, F, A} ->
                {1, 0, apply(M, F, A)};
            {F, A} ->
                {1, 0, apply(F, A)}
        end
    catch
        Error:Reason:Stacktrace ->
            Format = "Job ~p in process ~p with result:~n~p",
            Message = lists:flatten(io_lib:format(
                Format,
                [Error, self(), {Reason, Stacktrace}])),
            logger:error(Message),
            {0, 1, {Error, Reason}}
    end.

time_diff_ms(CurrentDateTime, NextDateTime) ->
    CurrentSeconds = calendar:datetime_to_gregorian_seconds(CurrentDateTime),
    NextSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
    (NextSeconds - CurrentSeconds) * 1000.

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
        case valid_datetime(MonthSpec, Month, false) of
            false when Month =:= 12 ->
                {forward, {{Year + 1, 1, 1}, {0, 0, 0}}};
            false ->
                {forward, {{Year, Month + 1, 1}, {0, 0, 0}}};
            true ->
                DayOfWeek = case calendar:day_of_the_week(Year, Month, Day) of 7 -> 0; DOW -> DOW end,
                DOMValid = valid_datetime(DayOfMonthSpec, Day, false),
                DOWValid = valid_datetime(DayOfWeekSpec, DayOfWeek, false),
                case (DOMValid andalso DOWValid) orelse
                    ((DayOfMonthSpec =/= '*') andalso
                        (DayOfWeekSpec =/= '*') andalso
                        (DOMValid orelse DOWValid))
                of
                    false -> {forward, forward_day(DateTime)};
                    true ->
                        case valid_datetime(HourSpec, Hour, false) of
                            false -> {forward, forward_hour(DateTime)};
                            true ->
                                case valid_datetime(MinuteSpec, Minute, false) of
                                    false -> {forward, forward_min(DateTime)};
                                    true ->
                                        case valid_datetime(SecondSpec, Second, false) of
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

valid_datetime('*', _Value, _) -> true;
valid_datetime(_, _Value, true) -> true;
valid_datetime([], _Value, false) -> false;
valid_datetime([Value | _T], Value, false) -> true;
valid_datetime([{Lower, Upper} | _], Value, false) when Lower =< Value andalso Value =< Upper -> true;
valid_datetime([List | T], Value, false) when is_list(List) -> valid_datetime(T, Value, lists:member(Value, List));
valid_datetime([_ | T], Value, false) -> valid_datetime(T, Value, false).

count_down_to_activate(StartTime, TimeType) ->
    {StartMs, Msg} =
        case StartTime of
            unlimited -> {0, ?Activate};
            _ ->
                case time_diff_ms(current_datetime(TimeType), StartTime) of
                    Ms when Ms > 16#ffffffff -> {16#ffffffff, ?Countdown_to_activate};
                    Ms when Ms > 0 -> {Ms, ?Activate};
                    _ -> {0, ?Activate}
                end
        end,
    erlang:send_after(StartMs, self(), Msg).

predict_cron_datetime(_Start, _End, _Job, _Now, 0, Acc) -> lists:reverse(Acc);
predict_cron_datetime(Start, End, Job, {TimeType, Now}, Num, Acc) ->
    Next = next_schedule_datetime(Job, Now),
    case in_range(Next, Start, End) of
        ?Already_ended -> lists:reverse(Acc);
        ?Deactivate -> predict_cron_datetime(Start, End, Job, {TimeType, Start}, Num, Acc);
        ?Running ->
            predict_cron_datetime(Start, End, Job, {TimeType, Next},
                Num - 1, [to_rfc3339(TimeType, Next) | Acc])
    end.

predict_every_datetime(_Start, _End, _Second, _Now, 0, Acc) -> lists:reverse(Acc);
predict_every_datetime(Start, End, Second, {TimeType, Now}, Num, Acc) ->
    FSeconds = calendar:datetime_to_gregorian_seconds(Now) + Second,
    Next = calendar:gregorian_seconds_to_datetime(FSeconds),
    case in_range(Next, Start, End) of
        ?Already_ended -> lists:reverse(Acc);
        ?Deactivate -> predict_every_datetime(Start, End, Second, {TimeType, Start}, Num, Acc);
        ?Running ->
            predict_every_datetime(Start, End, Second, {TimeType, Next}, Num - 1,
                [to_rfc3339(TimeType, Next) | Acc])
    end.

in_range(_Current, unlimited, unlimited) -> ?Running;
in_range(Current, unlimited, End) ->
    case time_diff_ms(End, Current) > 0 of
        true -> ?Already_ended;
        false -> ?Running
    end;
in_range(Current, Start, unlimited) ->
    case time_diff_ms(Start, Current) < 0 of
        true -> ?Deactivate;
        false -> ?Running
    end;
in_range(Current, Start, End) ->
    case time_diff_ms(End, Current) > 0 of
        true -> ?Already_ended;
        false ->
            case time_diff_ms(Start, Current) < 0 of
                true -> ?Deactivate;
                false -> ?Running
            end
    end.

current_datetime(local) -> calendar:local_time();
current_datetime(utc) -> calendar:universal_time().

-define(SECONDS_FROM_0_TO_1970, 719528 * 86400).

to_rfc3339(_TimeType, unlimited) -> unlimited;
to_rfc3339(TimeType, Time) ->
    UniversalTime =
        case TimeType of
            utc -> Time;
            local ->
                [T1 | _] = calendar:local_time_to_universal_time_dst(Time),
                T1
        end,
    SystemTime = calendar:datetime_to_gregorian_seconds(UniversalTime) - ?SECONDS_FROM_0_TO_1970,
    calendar:system_time_to_rfc3339(SystemTime).

%% For PropEr Test
-ifdef(TEST).
-compile(export_all).
-endif.
