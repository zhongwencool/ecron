%%% @private
-module(ecron_tick).
-include("ecron.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-export([add/2, delete/1]).
-export([activate/1, deactivate/1]).
-export([statistic/1, statistic/0]).
-export([predict_datetime/2]).

-export([start_link/1, handle_call/3, handle_info/2, init/1, handle_cast/2]).
-export([spawn_mfa/2]).

-record(state, {time_zone, max_timeout}).
-record(job, {name, status = activate, job, opts = [], ok = 0, failed = 0,
    link = undefined, result = [], run_microsecond = []}).
-record(timer, {key, name, cur_count = 0, singleton, type, spec, mfa, link,
    start_sec = unlimited, end_sec = unlimited, max_count = unlimited}).
-define(Timer, ecron_timer).

-define(MAX_SIZE, 16).
-define(SECONDS_FROM_0_TO_1970, 719528 * 86400).
-define(day_of_week(Y, M, D), (case calendar:day_of_the_week(Y, M, D) of 7 -> 0; D1 -> D1 end)).
-define(MatchSpec(Name), [{#timer{name = '$1', _ = '_'}, [], [{'=:=', '$1', {const, Name}}]}]).

add(Job, Options) -> gen_server:call(?Ecron, {add, Job, Options}, infinity).
delete(Name) -> gen_server:call(?Ecron, {delete, Name}, infinity).
activate(Name) -> gen_server:call(?Ecron, {activate, Name}, infinity).
deactivate(Name) -> gen_server:call(?Ecron, {deactivate, Name}, infinity).
get_next_schedule_time(Name) -> gen_server:call(?Ecron, {next_schedule_time, Name}, infinity).
clear() -> gen_server:call(?Ecron, clear, infinity).

statistic(Name) ->
    case ets:lookup(?Job, Name) of
        [] -> {error, not_found};
        [Job] -> {ok, job_to_statistic(Job, get_time_zone())}
    end.

statistic() ->
    TZ = get_time_zone(),
    [begin job_to_statistic(Job, TZ) end || Job <- ets:tab2list(?Job)].

predict_datetime(Job, Num) ->
    predict_datetime(activate, Job, unlimited, unlimited, Num, get_time_zone()).

start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    TZ = get_time_zone(),
    ?Timer = ets:new(?Timer, [named_table, ordered_set, private, {keypos, #timer.key}]),
    MaxTimeout = application:get_env(?Ecron, adjusting_time_second, 7 * 24 * 3600) * 1000,
    case reload_crontab() of
        ok ->
            [begin add_job(Job, TZ, Opts, true)
             end || #job{job = Job, opts = Opts, status = activate} <- ets:tab2list(?Job)],
            State = #state{
                max_timeout = MaxTimeout,
                time_zone = TZ},
            {ok, State, next_timeout(State)};
        Reason -> Reason
    end.

handle_call({add, Job, Options}, _From, #state{time_zone = TZ} = State) ->
    {reply, add_job(Job, TZ, Options, false), State, tick(State)};

handle_call({delete, Name}, _From, State) ->
    delete_job(Name),
    {reply, ok, State, next_timeout(State)};

handle_call({activate, Name}, _From, #state{time_zone = TZ} = State) ->
    {reply, activate_job(Name, TZ), State, tick(State)};

handle_call({deactivate, Name}, _From, State) ->
    {reply, deactivate_job(Name), State, next_timeout(State)};

handle_call({next_schedule_time, Name}, _From, State) ->
    %% P = ets:fun2ms(fun(#timer{name = N, key = {Time, _}}) when N =:= Name -> Time end),
    P = [{#timer{key = {'$1', '_'}, name = '$2', _ = '_'}, [{'=:=', '$2', {const, Name}}], ['$1']}],
    Res = ets:select(?Timer, P),
    {reply, Res, State, next_timeout(State)};

handle_call(clear, _From, State) ->
    ets:delete_all_objects(?Timer),
    ets:delete_all_objects(?Job),
    {reply, ok, State, next_timeout(State)};

handle_call(_Unknown, _From, State) ->
    {noreply, State, next_timeout(State)}.

handle_info(timeout, State) ->
    {noreply, State, tick(State)};

handle_info({'EXIT', Pid, _Reason}, State) ->
    pid_delete(Pid),
    {noreply, State, next_timeout(State)};

handle_info(_Unknown, State) ->
    {noreply, State, next_timeout(State)}.

handle_cast(_Unknown, State) ->
    {noreply, State, next_timeout(State)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
reload_crontab() ->
    reload_crontab(application:get_env(ecron, jobs, [])).

reload_crontab([]) -> ok;
reload_crontab([{Name, Spec, {_M, _F, _A} = MFA} | Jobs]) ->
    reload_crontab([{Name, Spec, {_M, _F, _A} = MFA, unlimited, unlimited, []} | Jobs]);
reload_crontab([{Name, Spec, {_M, _F, _A} = MFA, Start, End} | Jobs]) ->
    reload_crontab([{Name, Spec, {_M, _F, _A} = MFA, Start, End, []} | Jobs]);
reload_crontab([{Name, Spec, {_M, _F, _A} = MFA, Start, End, Opts} | Jobs]) ->
    case parse_job(Name, Spec, MFA, Start, End, Opts) of
        {ok, Job} ->
            case ets:lookup(?Job, Name) of
                [] -> ets:insert(?Job, Job);
                _ -> ok
            end,
            reload_crontab(Jobs);
        {error, Field, Reason} -> {stop, lists:flatten(io_lib:format("~p: ~p", [Field, Reason]))}
    end;
reload_crontab([L | _]) -> {stop, L}.

parse_job(JobName, Spec, MFA, Start, End, Opts) ->
    case ecron:valid_datetime(Start, End) of
        true ->
            case ecron:parse_spec(Spec) of
                {ok, Type, Crontab} ->
                    Job = #{type => Type, name => JobName, crontab => Crontab, mfa => MFA,
                        start_time => Start, end_time => End},
                    {ok, #job{name = JobName,
                        status = activate, job = Job,
                        opts = valid_opts(Opts),
                        link = link_pid(MFA)}};
                ErrParse -> ErrParse
            end;
        false ->
            {error, invalid_time, {Start, End}}
    end.

add_job(#{name := Name, mfa := MFA} = Job, TZ, Opts, NewJob) ->
    NewOpts = valid_opts(Opts),
    Pid = link_pid(MFA),
    JobRec = #job{status = activate, name = Name, job = Job, opts = NewOpts, link = Pid},
    Insert = ets:insert_new(?Job, JobRec),
    update_timer(Insert orelse NewJob, TZ, NewOpts, Job, current_millisecond(), Pid).

activate_job(Name, TZ) ->
    case ets:lookup(?Job, Name) of
        [] -> {error, not_found};
        [#job{job = Job, opts = Opts}] ->
            delete_job(Name),
            case add_job(Job, TZ, Opts, false) of
                {ok, Name} -> ok;
                Err -> Err
            end
    end.

deactivate_job(Name) ->
    ets:select_delete(?Timer, ?MatchSpec(Name)),
    case ets:update_element(?Job, Name, {#job.status, deactivate}) of
        true -> ok;
        false -> {error, not_found}
    end.

delete_job(Name) ->
    ets:select_delete(?Timer, ?MatchSpec(Name)),
    case ets:lookup(?Job, Name) of
        [] -> ok;
        [#job{link = Link}] ->
            unlink_pid(Link),
            ets:delete(?Job, Name)
    end.

update_timer(false, _, _, _, _, _) -> {error, already_exist};
update_timer(true, TZ, Opts, Job, Now, LinkPid) ->
    Singleton = proplists:get_value(singleton, Opts),
    MaxCount = proplists:get_value(max_count, Opts),
    #{name := Name, crontab := Spec, type := Type, mfa := MFA,
        start_time := StartTime, end_time := EndTime} = Job,
    Start = datetime_to_millisecond(TZ, StartTime),
    End = datetime_to_millisecond(TZ, EndTime),
    case next_schedule_millisecond(Type, Spec, TZ, Now, Start, End) of
        {ok, NextSec} ->
            Timer = #timer{key = {NextSec, Name}, singleton = Singleton,
                name = Name, type = Type, spec = Spec, max_count = MaxCount,
                mfa = MFA, start_sec = Start, end_sec = End, link = LinkPid},
            ets:insert(?Timer, Timer),
            {ok, Name};
        {error, already_ended} = Err ->
            delete_job(Name),
            Err
    end.

current_millisecond() -> erlang:system_time(millisecond).

datetime_to_millisecond(_, unlimited) -> unlimited;
datetime_to_millisecond(local, DateTime) ->
    UtcTime = erlang:localtime_to_universaltime(DateTime),
    datetime_to_millisecond(utc, UtcTime);
datetime_to_millisecond(utc, DateTime) ->
    (calendar:datetime_to_gregorian_seconds(DateTime) - ?SECONDS_FROM_0_TO_1970) * 1000.

millisecond_to_datetime(local, Ms) -> calendar:system_time_to_local_time(Ms, millisecond);
millisecond_to_datetime(utc, Ms) -> calendar:system_time_to_universal_time(Ms, millisecond).

spawn_mfa(Name, MFA) ->
    Start = erlang:monotonic_time(),
    {OkInc, FailedInc, NewRes} =
        try
            case MFA of
                {erlang, send, [Pid, Message]} ->
                    erlang:send(Pid, Message),
                    {1, 0, ok};
                {M, F, A} -> {1, 0, apply(M, F, A)};
                {F, A} -> {1, 0, apply(F, A)}
            end
        catch
            Error:Reason:Stacktrace ->
                logger:error("Job[~p] ~p with result:~n~p",
                    [Name, Error, {Reason, Stacktrace}]),
                {0, 1, {Error, Reason}}
        end,
    End = erlang:monotonic_time(),
    Cost = erlang:convert_time_unit(End - Start, native, microsecond),
    case ets:lookup(?Job, Name) of
        [] -> ok;
        [Job] ->
            #job{ok = Ok, failed = Failed, run_microsecond = RunMs, result = Results} = Job,
            Elements = [{#job.ok, Ok + OkInc}, {#job.failed, Failed + FailedInc},
                {#job.run_microsecond, lists:sublist([Cost | RunMs], ?MAX_SIZE)},
                {#job.result, lists:sublist([NewRes | Results], ?MAX_SIZE)}],
            ets:update_element(?Job, Name, Elements)
    end.

tick(State) ->
    tick_tick(ets:first(?Timer), current_millisecond(), State).

tick_tick('$end_of_table', _Cur, _State) -> infinity;
tick_tick({Due, _Name}, Cur, #state{max_timeout = MaxTimeout}) when Due > Cur ->
    min(Due - Cur, MaxTimeout);
tick_tick(Key = {_, Name}, Cur, State) ->
    #state{time_zone = TZ} = State,
    [Cron = #timer{singleton = Singleton, mfa = MFA, max_count = MaxCount, cur_count = CurCount}] = ets:lookup(?Timer, Key),
    ets:delete(?Timer, Key),
    {Incr, CurPid} = maybe_spawn_worker(Singleton, Name, MFA),
    update_next_schedule(CurCount + Incr, MaxCount, Cron, Cur, Name, TZ, CurPid),
    tick(State).

update_next_schedule(Max, Max, _Cron, _Cur, Name, _TZ, _CurPid) -> delete_job(Name);
update_next_schedule(Count, _Max, Cron, Cur, Name, TZ, CurPid) ->
    #timer{type = Type, start_sec = Start, end_sec = End, spec = Spec} = Cron,
    case next_schedule_millisecond(Type, Spec, TZ, Cur, Start, End) of
        {ok, Next} ->
            NextTimer = Cron#timer{key = {Next, Name}, singleton = CurPid, cur_count = Count},
            ets:insert(?Timer, NextTimer);
        {error, already_ended} ->
            delete_job(Name)
    end.

next_schedule_millisecond(every, Sec, _TimeZone, Now, Start, End) ->
    Next = Now + Sec * 1000,
    case in_range(Next, Start, End) of
        {error, deactivate} -> {ok, Start};
        {error, already_ended} -> {error, already_ended};
        ok -> {ok, Next}
    end;
next_schedule_millisecond(cron, Spec, TimeZone, Now, Start, End) ->
    ForwardDateTime = millisecond_to_datetime(TimeZone, Now + 1000),
    DefaultMin = #{second => 0, minute => 0, hour => 0,
        day_of_month => 1, month => 1, day_of_week => 0},
    Min = spec_min(maps:to_list(Spec), DefaultMin),
    NextDateTime = next_schedule_datetime(Spec, Min, ForwardDateTime),
    Next = datetime_to_millisecond(TimeZone, NextDateTime),
    case in_range(Next, Start, End) of
        {error, deactivate} -> next_schedule_millisecond(cron, Spec, TimeZone, Start, Start, End);
        {error, already_ended} -> {error, already_ended};
        ok -> {ok, Next}
    end.

next_schedule_datetime(DateSpec, Min, DateTime) ->
    #{
        second := SecondSpec, minute := MinuteSpec, hour := HourSpec,
        day_of_month := DayOfMonthSpec, month := MonthSpec,
        day_of_week := DayOfWeekSpec} = DateSpec,
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    case valid_datetime(MonthSpec, Month) of
        false -> forward_month(DateTime, Min, DateSpec);
        true ->
            case valid_day(Year, Month, Day, DayOfMonthSpec, DayOfWeekSpec) of
                false ->
                    LastDay = calendar:last_day_of_the_month(Year, Month),
                    forward_day(DateTime, Min, LastDay, DateSpec);
                true ->
                    case valid_datetime(HourSpec, Hour) of
                        false -> forward_hour(DateTime, Min, DateSpec);
                        true ->
                            case valid_datetime(MinuteSpec, Minute) of
                                false -> forward_minute(DateTime, Min, DateSpec);
                                true ->
                                    case valid_datetime(SecondSpec, Second) of
                                        false -> forward_second(DateTime, Min, DateSpec);
                                        true -> DateTime
                                    end
                            end
                    end
            end
    end.

forward_second(DateTime, Min, Spec) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    NewSecond = nearest(second, Second, 59, Spec),
    case Second >= NewSecond of
        true -> forward_minute(DateTime, Min, Spec);
        false -> {{Year, Month, Day}, {Hour, Minute, NewSecond}}
    end.

forward_minute(DateTime, Min, Spec) ->
    {{Year, Month, Day}, {Hour, Minute, _Second}} = DateTime,
    NewMinute = nearest(minute, Minute, 59, Spec),
    case Minute >= NewMinute of
        true -> forward_hour(DateTime, Min, Spec);
        false ->
            #{second := SecondM} = Min,
            {{Year, Month, Day}, {Hour, NewMinute, SecondM}}
    end.

forward_hour(DateTime, Min, Spec) ->
    {{Year, Month, Day}, {Hour, _Minute, _Second}} = DateTime,
    NewHour = nearest(hour, Hour, 23, Spec),
    case Hour >= NewHour of
        true ->
            LastDay = calendar:last_day_of_the_month(Year, Month),
            forward_day(DateTime, Min, LastDay, Spec);
        false ->
            #{minute := MinuteM, second := SecondM} = Min,
            {{Year, Month, Day}, {NewHour, MinuteM, SecondM}}
    end.

forward_day(DateTime, Min, LastDay, Spec) ->
    {{Year, Month, Day}, {_Hour, _Minute, _Second}} = DateTime,
    case Day + 1 of
        NewDay when NewDay > LastDay -> forward_month(DateTime, Min, Spec);
        NewDay ->
            #{hour := HourM, minute := MinuteM, second := SecondM} = Min,
            NewDateTime = {{Year, Month, NewDay}, {HourM, MinuteM, SecondM}},
            #{day_of_week := DayOfWeekSpec, day_of_month := DayOfMonthSpec} = Spec,
            case valid_day(Year, Month, NewDay, DayOfMonthSpec, DayOfWeekSpec) of
                true -> NewDateTime;
                false -> forward_day(NewDateTime, Min, LastDay, Spec)
            end
    end.

forward_month(DateTime, Min, Spec) ->
    {{Year, Month, _Day}, {_Hour, _Minute, _Second}} = DateTime,
    NewMonth = nearest(month, Month, 12, Spec),
    #{month := MonthM, hour := HourM, minute := MinuteM, second := SecondM} = Min,
    NewDateTime =
        {{NYear, NMonth, NDay}, {_NHour, _NMinute, _NSecond}} =
        case Month >= NewMonth of
            true -> {{Year + 1, MonthM, 1}, {HourM, MinuteM, SecondM}};
            false -> {{Year, NewMonth, 1}, {HourM, MinuteM, SecondM}}
        end,
    #{day_of_week := DayOfWeekSpec, day_of_month := DayOfMonthSpec} = Spec,
    case valid_day(NYear, NMonth, NDay, DayOfMonthSpec, DayOfWeekSpec) of
        false ->
            LastDay = calendar:last_day_of_the_month(NYear, NMonth),
            forward_day(NewDateTime, Min, LastDay, Spec);
        true -> NewDateTime
    end.

nearest(Type, Current, Max, Spec) ->
    Values = maps:get(Type, Spec),
    nearest_1(Values, Values, Max, Current + 1).

nearest_1('*', '*', MaxLimit, Next) when Next > MaxLimit -> 1;
nearest_1('*', '*', _MaxLimit, Next) -> Next;
nearest_1([], [{Min, _} | _], _Max, _Next) -> Min;
nearest_1([], [Min | _], _Max, _Next) -> Min;
nearest_1([{Min, Max} | Rest], Spec, MaxLimit, Next) ->
    if
        Next > Max -> nearest_1(Rest, Spec, MaxLimit, Next);
        Next =< Min -> Min;
        true -> Next
    end;
nearest_1([Expect | Rest], Spec, MaxLimit, Next) ->
    case Next > Expect of
        true -> nearest_1(Rest, Spec, MaxLimit, Next);
        false -> Expect
    end.

valid_datetime('*', _Value) -> true;
valid_datetime([], _Value) -> false;
valid_datetime([Value | _T], Value) -> true;
valid_datetime([{Lower, Upper} | _], Value) when Lower =< Value andalso Value =< Upper -> true;
valid_datetime([_ | T], Value) -> valid_datetime(T, Value).

valid_day(_Year, _Month, _Day, '*', '*') -> true;
valid_day(_Year, _Month, Day, DayOfMonthSpec, '*') ->
    valid_datetime(DayOfMonthSpec, Day);
valid_day(Year, Month, Day, '*', DayOfWeekSpec) ->
    DayOfWeek = ?day_of_week(Year, Month, Day),
    valid_datetime(DayOfWeekSpec, DayOfWeek);
valid_day(Year, Month, Day, DayOfMonthSpec, DayOfWeekSpec) ->
    case valid_datetime(DayOfMonthSpec, Day) of
        false ->
            DayOfWeek = ?day_of_week(Year, Month, Day),
            valid_datetime(DayOfWeekSpec, DayOfWeek);
        true -> true
    end.

spec_min([], Acc) -> Acc;
spec_min([{Key, Value} | Rest], Acc) ->
    NewAcc =
        case Value of
            '*' -> Acc;
            [{Min, _} | _] -> Acc#{Key => Min};
            [Min | _] -> Acc#{Key => Min}
        end,
    spec_min(Rest, NewAcc).

next_timeout(#state{max_timeout = MaxTimeout}) ->
    case ets:first(?Timer) of
        '$end_of_table' -> infinity;
        {Due, _} -> min(max(Due - current_millisecond(), 0), MaxTimeout)
    end.

in_range(_Current, unlimited, unlimited) -> ok;
in_range(Current, unlimited, End) when Current > End -> {error, already_ended};
in_range(_Current, unlimited, _End) -> ok;
in_range(Current, Start, unlimited) when Current < Start -> {error, deactivate};
in_range(_Current, _Start, unlimited) -> ok;
in_range(Current, _Start, End) when Current > End -> {error, already_ended};
in_range(Current, Start, _End) when Current < Start -> {error, deactivate};
in_range(_Current, _Start, _End) -> ok.

to_rfc3339(unlimited) -> unlimited;
to_rfc3339(Next) -> calendar:system_time_to_rfc3339(Next div 1000, [{unit, second}]).

job_to_statistic(Job, TimeZone) ->
    #job{job = JobSpec, status = Status, opts = Opts,
        ok = Ok, failed = Failed, result = Res, run_microsecond = RunMs} = Job,
    #{start_time := StartTime, end_time := EndTime} = JobSpec,
    Start = datetime_to_millisecond(TimeZone, StartTime),
    End = datetime_to_millisecond(TimeZone, EndTime),
    JobSpec#{status => Status, ok => Ok, failed => Failed, opts => Opts,
        next => predict_datetime(Status, JobSpec, Start, End, ?MAX_SIZE, TimeZone),
        start_time => to_rfc3339(datetime_to_millisecond(TimeZone, StartTime)),
        end_time => to_rfc3339(datetime_to_millisecond(TimeZone, EndTime)),
        results => Res, run_microsecond => RunMs}.

predict_datetime(deactivate, _, _, _, _, _) -> [];
predict_datetime(activate, #{type := every, crontab := Sec} = Job, Start, End, Num, TimeZone) ->
    Now =
        case maps:find(name, Job) of
            error -> current_millisecond();
            {ok, Name} ->
                [N] = get_next_schedule_time(Name),
                N - Sec * 1000
        end,
    predict_datetime(Job, TimeZone, Now, Start, End, Num, []);
predict_datetime(activate, Job, Start, End, Num, TimeZone) ->
    Now = current_millisecond(),
    predict_datetime(Job, TimeZone, Now, Start, End, Num, []).

predict_datetime(_Job, _TimeZone, _Now, _Start, _End, 0, Acc) -> lists:reverse(Acc);
predict_datetime(Job, TimeZone, Now, Start, End, Num, Acc) ->
    #{type := Type, crontab := Spec} = Job,
    case next_schedule_millisecond(Type, Spec, TimeZone, Now, Start, End) of
        {ok, Next} ->
            NewAcc = [to_rfc3339(Next) | Acc],
            predict_datetime(Job, TimeZone, Next, Start, End, Num - 1, NewAcc);
        {error, already_ended} -> lists:reverse(Acc)
    end.

get_time_zone() -> application:get_env(?Ecron, time_zone, local).

maybe_spawn_worker(_, Name, {erlang, send, Args}) ->
    {1, spawn_mfa(Name, {erlang, send, Args})};
maybe_spawn_worker(true, Name, MFA) ->
    {1, spawn(?MODULE, spawn_mfa, [Name, MFA])};
maybe_spawn_worker(false, Name, MFA) ->
    spawn(?MODULE, spawn_mfa, [Name, MFA]),
    {1, false};
maybe_spawn_worker(Pid, Name, MFA) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true -> {0, Pid};
        false -> {1, spawn(?MODULE, spawn_mfa, [Name, MFA])}
    end.

pid_delete(Pid) ->
    TimerMatch = [{#timer{link = '$1', _ = '_'}, [], [{'=:=', '$1', {const, Pid}}]}],
    JobMatch = [{#job{link = '$1', _ = '_'}, [], [{'=:=', '$1', {const, Pid}}]}],
    ets:select_delete(?Timer, TimerMatch),
    ets:select_delete(?Job, JobMatch).

valid_opts(Opts) ->
    Singleton = proplists:get_value(singleton, Opts, true),
    MaxCount = proplists:get_value(max_count, Opts, unlimited),
    [{singleton, Singleton}, {max_count, MaxCount}].
link_pid({erlang, send, [PidOrName, _Message]}) ->
    Pid = get_pid(PidOrName),
    is_pid(Pid) andalso (catch link(Pid)),
    Pid;
link_pid(_MFA) -> undefined.

unlink_pid(Pid) when is_pid(Pid) -> catch unlink(Pid);
unlink_pid(_) -> ok.

get_pid(Pid) when is_pid(Pid) -> Pid;
get_pid(Name) when is_atom(Name) -> whereis(Name).

%% For PropEr Test
-ifdef(TEST).
-compile(export_all).
-endif.
