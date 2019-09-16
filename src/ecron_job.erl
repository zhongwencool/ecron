-module(ecron_job).
-behaviour(gen_server).

-export([predict_datetime/3]).

-export([activate/1, deactivate/1]).
-export([statistic/1]).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(MAX_SIZE, 20).

-define(AlreadyEnded, already_ended).
-define(Waiting, waiting).
-define(Running, running).
-define(Deactivate, deactivate).
-define(Activate, activate).

-define(Countdown_to_activate, countdown_to_activate).
-define(Stop, stop).
-define(Statistic, statistic).
-define(Instant, instant).

start_link(Ecron) -> gen_server:start_link(?MODULE, [Ecron], []).
stop(Pid) -> gen_server:call(Pid, ?Stop).
activate(Pid) -> gen_server:call(Pid, ?Activate).
deactivate(Pid) -> gen_server:call(Pid, ?Deactivate).
statistic(Pid) -> gen_server:call(Pid, ?Statistic).

init([Ecron]) ->
    #{start_time := StartTime, name := Name} = Ecron,
    TimeZone = application:get_env(ecron, time_zone, local),
    MaxTimeout = application:get_env(ecron, adjusting_time_second, 7 * 24 * 3600),
    case ets:insert_new(ecron, {Name, self()}) of
        true ->
            State = #{
                worker => undefined, ecron => Ecron, adjusting_time_second => MaxTimeout,
                status => ?Deactivate, results => [],
                run_microsecond => [], time_zone => TimeZone,
                ok => 0, failed => 0},
            erlang:process_flag(trap_exit, true),
            {ok, maybe_activate(StartTime, State)};
        false ->
            {stop, {shutdown, already_exist}}
    end.

handle_call(?Activate, _From, State = #{worker := Worker}) ->
    erlang:is_pid(Worker) andalso erlang:exit(Worker, ?Deactivate),
    {reply, ok, do_activate(State#{status =>?Deactivate, worker => undefined})};

handle_call(?Deactivate, _From, State = #{worker := Worker}) ->
    erlang:is_pid(Worker) andalso erlang:exit(Worker, ?Deactivate),
    {reply, ok, State#{status => ?Deactivate, worker => undefined}};

handle_call(?Statistic, _From, State = #{ecron := Ecron, time_zone := TimeZone}) ->
    Next = predict_datetime(Ecron, {TimeZone, current_datetime(TimeZone)}, 10),
    #{start_time := StartTime, end_time := EndTime} = Ecron,
    {reply,
        {ok, State#{
            next => Next,
            ecron =>
            Ecron#{start_time => to_rfc3339(TimeZone, StartTime),
                end_time => to_rfc3339(TimeZone, EndTime)}}
        },
        State};

handle_call(?Stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, error, State}.

handle_cast({update_status, Status, Worker}, State = #{status := OldStatus})
    when OldStatus =/= ?Deactivate andalso OldStatus =/= ?AlreadyEnded ->
    {noreply, State#{status => Status, worker => Worker}};
handle_cast({update_run_statistic, Status, Start, End, {OkInc, FailedInc, Result}},
    State = #{status := OldStatus})
    when OldStatus =/= ?Deactivate andalso OldStatus =/= ?AlreadyEnded ->
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

handle_info(?Countdown_to_activate, State = #{ecron := #{start_time := StartTime}}) ->
    {noreply, maybe_activate(StartTime, State)};
handle_info(?Activate, State = #{status := ?Deactivate}) ->
    {noreply, do_activate(State)};
handle_info({'EXIT', _OldWorker, ?Deactivate}, State) ->
    {noreply, State};
handle_info({'EXIT', _OldWorker, normal}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    logger:error("~p unknow message:~p~n ~p~n", [self(), Info, State]),
    {noreply, State}.

terminate(_Reason, #{ecron := #{name := JobName}, worker := Worker}) ->
    is_pid(Worker) andalso erlang:exit(Worker, killed),
    ets:delete(ecron, JobName),
    ok.

predict_datetime(#{type := every, crontab := Ms, start_time := Start, end_time := End},
    Now, Num) ->
    predict_every_datetime(Start, End, trunc(Ms / 1000), Now, Num, []);
predict_datetime(#{type := cron, crontab := Job, start_time := Start, end_time := End},
    Now, Num) ->
    predict_cron_datetime(Start, End, Job, Now, Num, []);
predict_datetime(Type, Job, Num) ->
    TimeZone = application:get_env(ecron, time_zone, local),
    Now = current_datetime(TimeZone),
    predict_datetime(#{type => Type, crontab => Job, start_time => unlimited, end_time => unlimited},
        {TimeZone, Now}, Num).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_activate(State = #{ecron := Ecron, status := ?Deactivate,
    time_zone := TimeZone, adjusting_time_second := MaxTimeout}) ->
    #{type := Type, mfa := MFA, crontab := Crontab, end_time := EndTime} = Ecron,
    EndSec = case EndTime of unlimited -> unlimited; _ -> calendar:datetime_to_gregorian_seconds(EndTime) end,
    Self = self(),
    WorkerFun =
        case Type of
            every -> fun() -> schedule_every(Crontab, MFA, EndSec, Self, TimeZone) end;
            cron ->
                fun() ->
                    schedule_cron(current_datetime(TimeZone),
                        Crontab, MFA, EndSec, Self, MaxTimeout, TimeZone) end
        end,
    State#{worker => spawn_link(WorkerFun), status => ?Waiting}.

schedule_every(Ms, MFA, EndSec, ParentPid, TimeZone) ->
    case unlimited =:= EndSec orelse
        calendar:datetime_to_gregorian_seconds(current_datetime(TimeZone)) < EndSec
    of
        true ->
            gen_server:cast(ParentPid, {update_status, ?Running, self()}),
            Start = erlang:monotonic_time(),
            Result = safe_apply_job(MFA),
            gen_server:cast(ParentPid, {update_run_statistic, ?Waiting, Start, erlang:monotonic_time(), Result}),
            timer:sleep(Ms),
            schedule_every(Ms, MFA, EndSec, ParentPid, TimeZone);
        false ->
            gen_server:cast(ParentPid, {update_status, ?AlreadyEnded, undefined})
    end.

schedule_cron(PrevDateTime, DateSpec, MFA, EndSec, ParentPid, MaxTimeout, TimeZone) ->
    CurrentDateTime = current_datetime(TimeZone),
    case unlimited =:= EndSec orelse
        calendar:datetime_to_gregorian_seconds(CurrentDateTime) < EndSec
    of
        true ->
            NextValidDateTime = next_schedule_datetime(DateSpec, CurrentDateTime, PrevDateTime),
            SleepSec = second_diff(CurrentDateTime, NextValidDateTime),
            gen_server:cast(ParentPid, {update_status, ?Waiting, self()}),
            case SleepSec > MaxTimeout of
                true ->
                    timer:sleep(MaxTimeout * 1000),
                    schedule_cron(CurrentDateTime, DateSpec, MFA, EndSec, ParentPid, MaxTimeout, TimeZone);
                false ->
                    timer:sleep(SleepSec * 1000),
                    gen_server:cast(ParentPid, {update_status, ?Running, self()}),
                    Start = erlang:monotonic_time(),
                    Res = safe_apply_job(MFA),
                    gen_server:cast(ParentPid,
                        {update_run_statistic, ?Waiting, Start, erlang:monotonic_time(), Res}),
                    schedule_cron(CurrentDateTime, DateSpec, MFA, EndSec, ParentPid, MaxTimeout, TimeZone)
            end;
        false ->
            gen_server:cast(ParentPid, {update_status, ?AlreadyEnded, undefined})
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

second_diff(CurrentDateTime, NextDateTime) ->
    CurrentSeconds = calendar:datetime_to_gregorian_seconds(CurrentDateTime),
    NextSeconds = calendar:datetime_to_gregorian_seconds(NextDateTime),
    NextSeconds - CurrentSeconds.

next_schedule_datetime(DateSpec, NowDateTime, PreDateTime) ->
    Incr = case NowDateTime =:= PreDateTime of true -> 1; false -> 0 end,
    FSeconds = calendar:datetime_to_gregorian_seconds(NowDateTime) + Incr,
    ForwardDateTime = calendar:gregorian_seconds_to_datetime(FSeconds),
    DefaultMin = #{second => 0, minute => 0, hour => 0, day_of_month => 1, month => 1, day_of_week => 0},
    Min = spec_min(maps:to_list(DateSpec), DefaultMin),
    next_schedule_datetime_1(DateSpec, Min, ForwardDateTime).

next_schedule_datetime_1(DateSpec, Min, DateTime) ->
    #{
        second := SecondSpec, minute := MinuteSpec, hour := HourSpec,
        day_of_month := DayOfMonthSpec, month := MonthSpec,
        day_of_week := DayOfWeekSpec} = DateSpec,
    
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    case valid_datetime(MonthSpec, Month) of
        false ->
            IncrMonth = nearest(month, Month, 12, DateSpec),
            forward_month(DateTime, IncrMonth, Min, DateSpec);
        true ->
            DayOfWeek = case calendar:day_of_the_week(Year, Month, Day) of 7 -> 0; DOW -> DOW end,
            DOMValid = valid_datetime(DayOfMonthSpec, Day),
            DOWValid = valid_datetime(DayOfWeekSpec, DayOfWeek),
            case (DOMValid andalso DOWValid) orelse
                ((DayOfMonthSpec =/= '*') andalso
                    (DayOfWeekSpec =/= '*') andalso
                    (DOMValid orelse DOWValid))
            of
                false ->
                    LastDay = calendar:last_day_of_the_month(Year, Month),
                    IncrDay = nearest(day_of_month, Day, LastDay, DateSpec),
                    IncrWeek = nearest(day_of_week, DayOfWeek, 6, DateSpec),
                    forward_day(DateTime, min(IncrDay, IncrWeek), Min, DateSpec);
                true ->
                    case valid_datetime(HourSpec, Hour) of
                        false ->
                            IncrHour = nearest(hour, Hour, 23, DateSpec),
                            forward_hour(DateTime, IncrHour, Min, DateSpec);
                        true ->
                            case valid_datetime(MinuteSpec, Minute) of
                                false ->
                                    IncrMinute = nearest(minute, Minute, 59, DateSpec),
                                    forward_minute(DateTime, IncrMinute, Min, DateSpec);
                                true ->
                                    case valid_datetime(SecondSpec, Second) of
                                        false ->
                                            IncrSec = nearest(second, Second, 59, DateSpec),
                                            forward_second(DateTime, IncrSec, Min, DateSpec);
                                        true -> DateTime
                                    end
                            end
                    end
            end
    end.

forward_second(DateTime, Incr, Min, Spec) when Incr > 0 ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    case Second + Incr of
        NewSecond when NewSecond > 59 ->
            IncrMinute = nearest(minute, Minute, 59, Spec),
            forward_minute(DateTime, IncrMinute, Min, Spec);
        NewSecond -> {{Year, Month, Day}, {Hour, Minute, NewSecond}}
    end.

forward_minute(DateTime, Incr, Min, Spec) when Incr > 0 ->
    {{Year, Month, Day}, {Hour, Minute, _Second}} = DateTime,
    #{second := SecondM} = Min,
    case Minute + Incr of
        NewMinute when NewMinute > 59 ->
            IncrHour = nearest(hour, Hour, 23, Spec),
            forward_hour(DateTime, IncrHour, Min, Spec);
        NewMinute -> {{Year, Month, Day}, {Hour, NewMinute, SecondM}}
    end.

forward_hour(DateTime, Incr, Min, Spec) when Incr > 0 ->
    {{Year, Month, Day}, {Hour, _Minute, _Second}} = DateTime,
    #{minute := MinuteM, second := SecondM} = Min,
    case Hour + Incr of
        NewHour when NewHour > 23 ->
            LastDay = calendar:last_day_of_the_month(Year, Month),
            IncrDay = nearest(day_of_month, Day, LastDay, Spec),
            DayOfWeek = case calendar:day_of_the_week(Year, Month, Day) of 7 -> 0; DOW -> DOW end,
            IncrWeek = nearest(day_of_week, DayOfWeek, 6, Spec),
            forward_day(DateTime, min(IncrDay, IncrWeek), Min, Spec);
        NewHour -> {{Year, Month, Day}, {NewHour, MinuteM, SecondM}}
    end.

forward_day(DateTime, Incr, Min, Spec) when Incr >= 0 ->
    {{Year, Month, Day}, {_Hour, _Minute, _Second}} = DateTime,
    #{hour := HourM, minute := MinuteM, second := SecondM} = Min,
    LastDay = calendar:last_day_of_the_month(Year, Month),
    case Day + Incr of
        NewDay when NewDay > LastDay ->
            IncrMonth = nearest(month, Month, 12, Spec),
            forward_month(DateTime, IncrMonth, Min, Spec);
        NewDay ->
            NewDateTime = {{Year, Month, NewDay}, {HourM, MinuteM, SecondM}},
            #{day_of_week := DayOfWeekSpec, day_of_month := DayOfMonthSpec} = Spec,
            case check_valid_day(Year, Month, NewDay, DayOfMonthSpec, DayOfWeekSpec) of
                true -> NewDateTime;
                false ->
                    IncrDay = nearest(day_of_month, NewDay, LastDay, Spec),
                    DayOfWeek = case calendar:day_of_the_week(Year, Month, NewDay) of 7 -> 0; DOW -> DOW end,
                    IncrWeek = nearest(day_of_week, DayOfWeek, 6, Spec),
                    forward_day(NewDateTime, min(IncrDay, IncrWeek), Min, Spec)
            end
    end.

check_valid_day(Year, Month, NDay, DayOfMonthSpec, DayOfWeekSpec) ->
    DayOfWeek = case calendar:day_of_the_week(Year, Month, NDay) of 7 -> 0; DOW -> DOW end,
    DOMValid = valid_datetime(DayOfMonthSpec, NDay),
    DOWValid = valid_datetime(DayOfWeekSpec, DayOfWeek),
    (DOMValid andalso DOWValid) orelse
        ((DayOfMonthSpec =/= '*') andalso
            (DayOfWeekSpec =/= '*') andalso
            (DOMValid orelse DOWValid)).


forward_month(DateTime, Incr, Min, Spec) when Incr > 0 ->
    {{Year, Month, _Day}, {_Hour, _Minute, _Second}} = DateTime,
    #{month := MonthM, hour := HourM, minute := MinuteM, second := SecondM} = Min,
    NewDateTime =
        {{NYear, NMonth, NDay}, {_NHour, _NMinute, _NSecond}} =
        case Month + Incr of
            NewMonth when NewMonth > 12 -> {{Year + 1, MonthM, 1}, {HourM, MinuteM, SecondM}};
            NewMonth -> {{Year, NewMonth, 1}, {HourM, MinuteM, SecondM}}
        end,
    #{day_of_week := DayOfWeekSpec, day_of_month := DayOfMonthSpec} = Spec,
    case check_valid_day(NYear, NMonth, NDay, DayOfMonthSpec, DayOfWeekSpec) of
        true -> NewDateTime;
        false ->
            LastDay = calendar:last_day_of_the_month(NYear, NMonth),
            IncrDay = nearest(day_of_month, NDay, LastDay, Spec),
            DayOfWeek = case calendar:day_of_the_week(NYear, NMonth, NDay) of 7 -> 0; DOW -> DOW end,
            IncrWeek = nearest(day_of_week, DayOfWeek, 6, Spec),
            forward_day(NewDateTime, min(IncrDay, IncrWeek), Min, Spec)
    end.

nearest(Type, Current, Max, Spec) ->
    Values = maps:get(Type, Spec),
    case nearest_1(Values, Values, Max, Current + 1) of
        {forward, Next} -> Max - Current + Next + 1;
        {done, Next} -> Next - Current
    end.

nearest_1('*', _, MaxLimit, Next) ->
    case Next > MaxLimit of
        true -> {forward, 1};
        false -> {done, Next}
    end;
nearest_1([], [{Min, _} | _], _Max, _Next) -> {forward, Min};
nearest_1([], [Min | _], _Max, _Next) -> {forward, Min};
nearest_1([{Min, Max} | Rest], Spec, MaxLimit, Next) ->
    if
        Next > Max -> nearest_1(Rest, Spec, MaxLimit, Next);
        Next =< Min -> {done, Min};
        true -> {done, Next}
    end;
nearest_1([Expect | Rest], Spec, MaxLimit, Next) ->
    if
        Next > Expect -> nearest_1(Rest, Spec, MaxLimit, Next);
        true -> {done, Expect}
    end.

valid_datetime('*', _Value) -> true;
valid_datetime([], _Value) -> false;
valid_datetime([Value | _T], Value) -> true;
valid_datetime([{Lower, Upper} | _], Value) when Lower =< Value andalso Value =< Upper -> true;
valid_datetime([_ | T], Value) -> valid_datetime(T, Value).

maybe_activate(StartTime, State = #{adjusting_time_second := MaxTimeout, time_zone := TimeZone}) ->
    case StartTime of
        unlimited -> do_activate(State);
        _ ->
            case second_diff(current_datetime(TimeZone), StartTime) of
                Sec when Sec > MaxTimeout ->
                    erlang:send_after(MaxTimeout * 1000, self(), ?Countdown_to_activate),
                    State;
                Sec when Sec > 0 ->
                    erlang:send_after(Sec * 1000, self(), ?Activate),
                    State;
                _ -> do_activate(State)
            end
    end.

predict_cron_datetime(_Start, _End, _Job, _Now, 0, Acc) -> lists:reverse(Acc);
predict_cron_datetime(Start, End, Job, {TimeZone, Now}, Num, Acc) ->
    Next = next_schedule_datetime(Job, Now, Now),
    case in_range(Next, Start, End) of
        ?AlreadyEnded -> lists:reverse(Acc);
        ?Deactivate -> predict_cron_datetime(Start, End, Job, {TimeZone, Start}, Num, Acc);
        ?Running ->
            predict_cron_datetime(Start, End, Job, {TimeZone, Next},
                Num - 1, [to_rfc3339(TimeZone, Next) | Acc])
    end.

predict_every_datetime(_Start, _End, _Second, _Now, 0, Acc) -> lists:reverse(Acc);
predict_every_datetime(Start, End, Second, {TimeZone, Now}, Num, Acc) ->
    FSeconds = calendar:datetime_to_gregorian_seconds(Now) + Second,
    Next = calendar:gregorian_seconds_to_datetime(FSeconds),
    case in_range(Next, Start, End) of
        ?AlreadyEnded -> lists:reverse(Acc);
        ?Deactivate -> predict_every_datetime(Start, End, Second, {TimeZone, Start}, Num, Acc);
        ?Running ->
            predict_every_datetime(Start, End, Second, {TimeZone, Next}, Num - 1,
                [to_rfc3339(TimeZone, Next) | Acc])
    end.

in_range(_Current, unlimited, unlimited) -> ?Running;
in_range(Current, unlimited, End) ->
    case second_diff(End, Current) > 0 of
        true -> ?AlreadyEnded;
        false -> ?Running
    end;
in_range(Current, Start, unlimited) ->
    case second_diff(Start, Current) < 0 of
        true -> ?Deactivate;
        false -> ?Running
    end;
in_range(Current, Start, End) ->
    case second_diff(End, Current) > 0 of
        true -> ?AlreadyEnded;
        false ->
            case second_diff(Start, Current) < 0 of
                true -> ?Deactivate;
                false -> ?Running
            end
    end.

current_datetime(local) -> calendar:local_time();
current_datetime(utc) -> calendar:universal_time().

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

spec_min([], Acc) -> Acc;
spec_min([{Key, Value} | Rest], Acc) ->
    NewAcc =
        case Value of
            '*' -> Acc;
            [{Min, _} | _] -> Acc#{Key => Min};
            [Min | _] -> Acc#{Key => Min}
        end,
    spec_min(Rest, NewAcc).

%% For PropEr Test
-ifdef(TEST).
-compile(export_all).
-endif.
