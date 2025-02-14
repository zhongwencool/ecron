-module(ecron).

-include("ecron.hrl").

?MODULEDOC("""
`ecron` is a lightweight and efficient cron-like job scheduling library.

Job Management:
  - `add/3`, `add/4`, `add/6`, `add/7`: Add cron jobs with various options
  - `add_with_time/5`: Add job with start/end time
  - `add_with_count/3`: Add job with execution count limit
  - `delete/1`, `delete/2`: Remove jobs
  - `deactivate/1`, `activate/1`: Pause/resume jobs

Timer Functions:
  - `send_interval/3`: Send message at fixed intervals
  - `send_after/3`: Send message after delay

Monitoring & Control:
  - `statistic/0`, `statistic/1`: Get job statistics
  - `reload/0`: Reload configuration
  - `parse_spec/2`: Parse cron expressions
""").
%% API Function
-export([add/3, add/4, add/6, add/7]).
-export([add_with_time/5, add_with_time/6]).
-export([add_with_count/3, add_with_count/5]).
-export([send_interval/3, send_interval/5, send_interval/7, send_interval/8]).
-export([send_after/3]).
-export([delete/1, delete/2]).
-export([deactivate/1, activate/1, deactivate/2, activate/2]).
-export([statistic/0, statistic/1, statistic/2]).
-export([reload/0]).
-export([parse_spec/2]).

%% Callback Function
-export([start_link/2, handle_call/3, handle_info/2, init/1, handle_cast/2]).
-export([spawn_mfa/3, clear/0]).

-record(state, {time_zone, max_timeout, timer_tab, job_tab}).
-record(timer, {
    key,
    name,
    cur_count = 0,
    singleton,
    type,
    spec,
    mfa,
    link,
    start_at = {0, 0, 0},
    end_at = {23, 59, 59},
    max_count = unlimited
}).

-define(MAX_SIZE, 16).
-define(SECONDS_FROM_0_TO_1970, 719528 * 86400).

-define(MatchNameSpec(Name), [{#timer{name = '$1', _ = '_'}, [], [{'=:=', '$1', {const, Name}}]}]).

%%%===================================================================
%%% API
%%%===================================================================
-type register() :: atom().
-type name() :: term().
-type crontab_spec() :: crontab() | string() | binary() | 1..4294967.

-type crontab() :: #{
    second => '*' | [0..59 | {0..58, 1..59}, ...],
    minute => '*' | [0..59 | {0..58, 1..59}, ...],
    hour => '*' | [0..23, ...],
    month => '*' | [1..12 | {1..11, 2..12}, ...],
    day_of_month => '*' | [1..31 | {1..30, 2..31}, ...],
    day_of_week => '*' | [0..6 | {0..5, 1..6}, ...]
}.

%% from calendar:rfc3339_string/0 exist but not exported
-type rfc3339_string() :: [byte(), ...].

-type mfargs() :: {M :: module(), F :: atom(), A :: [term()]}.
-type ecron() :: #{
    name => name(),
    crontab => crontab(),
    start_time => rfc3339_string() | unlimited,
    end_time => rfc3339_string() | unlimited,
    mfa => mfargs(),
    type => cron | every
}.

-type status() :: deactivate | activate.

-type statistic() :: #{
    ecron => ecron(),
    status => status(),
    failed => non_neg_integer(),
    ok => non_neg_integer(),
    results => [term()],
    run_microsecond => [pos_integer()],
    time_zone => local | utc,
    worker => pid(),
    next => [calendar:datetime()]
}.

-type parse_error() ::
    invalid_time | invalid_spec | month | day_of_month | day_of_week | hour | minute | second.

-type start_at() :: unlimited | calendar:time().
-type end_at() :: unlimited | calendar:time().
-type option() :: {singleton, boolean()} | {max_count, pos_integer() | unlimited}.
-type options() :: [option()].
-type ecron_result() :: {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.

?DOC("""
Same as [`add(ecron_local, JobName, Spec, MFA)`](`add/4`).
""").
-spec add(name(), crontab_spec(), mfargs()) -> ecron_result().
add(JobName, Spec, MFA) ->
    add(?LocalJob, JobName, Spec, MFA).

?DOC("""
Same as [`add(Register, JobName, Spec, MFA, unlimited, unlimited, [])`](`add/7`).
""").
-spec add(register(), name(), crontab_spec(), mfargs()) -> ecron_result().
add(Register, JobName, Spec, MFA) ->
    add(Register, JobName, Spec, MFA, unlimited, unlimited, []).

?DOC("""
Same as [`add_with_count(ecron_local, make_ref(), Spec, MFA, RunCount)`](`add_with_count/5`).
""").
-spec add_with_count(crontab_spec(), mfargs(), pos_integer()) -> ecron_result().
add_with_count(Spec, MFA, RunCount) when is_integer(RunCount) ->
    add_with_count(?LocalJob, make_ref(), Spec, MFA, RunCount).

?DOC("""
Same as [`add(Register, JobName, Spec, MFA, unlimited, unlimited, [{max_count, RunCount}])`](`add/7`).
""").
-spec add_with_count(register(), name(), crontab_spec(), mfargs(), pos_integer()) ->
    ecron_result().
add_with_count(Register, JobName, Spec, MFA, RunCount) when is_integer(RunCount) ->
    add(Register, JobName, Spec, MFA, unlimited, unlimited, [{max_count, RunCount}]).

?DOC("""
Same as [`add_with_time(ecron_local, JobName, Spec, MFA, Start, End)`](`add_with_time/6`).
""").
-spec add_with_time(name(), crontab_spec(), mfargs(), start_at(), end_at()) ->
    ecron_result().
add_with_time(JobName, Spec, MFA, Start, End) ->
    add_with_time(?LocalJob, JobName, Spec, MFA, Start, End).

?DOC("""
Same as [`add(Register, JobName, Spec, MFA, Start, End, [])`](`add/7`).
""").
-spec add_with_time(register(), name(), crontab_spec(), mfargs(), start_at(), end_at()) ->
    ecron_result().
add_with_time(Register, JobName, Spec, MFA, Start, End) ->
    add(Register, JobName, Spec, MFA, Start, End, []).

?DOC("""
Same as [`add(ecron_local, JobName, Spec, MFA, Start, End, Opts)`](`add/7`).
""").
-spec add(name(), crontab_spec(), mfargs(), start_at(), end_at(), options()) ->
    ecron_result().
add(JobName, Spec, MFA, Start, End, Opts) ->
    add(?LocalJob, JobName, Spec, MFA, Start, End, Opts).

?DOC("""
Adds a new crontab job with specified parameters. Jobs exceeding their limits are automatically removed.

Parameters:
* `Register` - The process name where the job will be registered
* `JobName` - Unique identifier for the job. Returns `{error, already_exist}` if duplicate
* `Spec` - Crontab expression defining execution schedule
* `MFA` - `{Module, Function, Args}` to execute when triggered
* `Start` - Start time `{Hour,Min,Sec}` or `unlimited` for immediate start
* `End` - End time `{Hour,Min,Sec}` or `unlimited` for no end
* `Opts` - Options list:
    * `{singleton, boolean()}` - If true (default), prevents concurrent execution
    * `{max_count, pos_integer() | unlimited}` - Maximum executions allowed

Returns: `{ok, JobName}` | `{error, already_exist}` | `{error, parse_error(), term()}`

> #### Singleton {: .info}
> When a job's singleton option is set to true, the system checks 
> if there is already an instance of the job running before starting a new execution. 
> If an instance is already running（old pid is alive）, the new execution will be skipped.

""").
-spec add(register(), name(), crontab_spec(), mfargs(), start_at(), end_at(), options()) ->
    ecron_result().
add(Register, JobName, Spec, MFA, Start, End, Opts) ->
    case ecron_spec:parse_start_end_time(Start, End) of
        {StartTime, EndTime} ->
            case ecron_spec:parse_spec(Spec) of
                {ok, Type, Crontab} ->
                    case ecron_spec:valid_time(Type, StartTime, EndTime, Crontab) of
                        true ->
                            Job = #{
                                type => Type,
                                name => JobName,
                                crontab => Crontab,
                                mfa => MFA,
                                start_time => StartTime,
                                end_time => EndTime
                            },
                            ValidOpts = ecron_spec:parse_valid_opts(Opts),
                            case gen_server:call(Register, {add, Job, ValidOpts}, infinity) of
                                {ok, Name} -> {ok, Name};
                                {error, already_exist} -> {error, already_exist}
                            end;
                        false ->
                            {error, invalid_time, {Start, End, Spec}}
                    end;
                ErrParse ->
                    ErrParse
            end;
        false ->
            {error, invalid_time, {Start, End, Spec}}
    end.

?DOC("""
Creates a one-time timer that sends a message when the crontab spec is triggered.

Parameters:
* `Spec` - Crontab expression defining when to trigger
* `Dest` - Destination pid() or registered name to receive message
* `Message` - Term to send when timer triggers

Notes:
* Similar to erlang:send_after/3 but uses crontab format
* Dest pid must be local
* Maximum time value is 4294967295 milliseconds
* Timer auto-cancels if destination process dies
* Use erlang:cancel_timer/1 to cancel, not ecron:delete/1

Returns: `{ok, reference()}` | `{error, parse_error(), term()}`
""").
-spec send_after(crontab_spec(), pid() | atom(), term()) ->
    {ok, reference()} | {error, parse_error(), term()}.
send_after(Spec, Pid, Message) ->
    case ecron_spec:parse_spec(Spec) of
        {ok, Type, JobSpec} ->
            TimeZone = get_time_zone(),
            Now = current_millisecond(),
            Next = next_schedule_millisecond(Type, JobSpec, TimeZone, Now, {0, 0, 0}, {23, 59, 59}),
            {ok, erlang:send_after(Next - Now, Pid, Message)};
        {error, _Field, _Value} = Error ->
            Error
    end.

?DOC("""
Same as [`send_interval(ecron_local, make_ref(), Spec, Pid, Message, unlimited, unlimited, [])`](`send_interval/7`).
""").
-spec send_interval(crontab_spec(), pid(), term()) -> ecron_result().
send_interval(Spec, Pid, Message) ->
    send_interval(?LocalJob, make_ref(), Spec, Pid, Message).

?DOC("""
Same as [`send_interval(Register, Name, Spec, Pid, Message, unlimited, unlimited, [])`](`send_interval/7`).
""").
-spec send_interval(register(), name(), crontab_spec(), pid(), term()) -> ecron_result().
send_interval(Register, Name, Spec, Pid, Message) ->
    send_interval(Register, Name, Spec, Pid, Message, unlimited, unlimited, []).

?DOC("""
Same as [`send_interval(register(), name(), Spec, self(), Message, Start, End, Option)`](`send_interval/8`).
""").
-spec send_interval(register(), name(), crontab_spec(), term(), start_at(), end_at(), options()) ->
    ecron_result().
send_interval(Register, Name, Spec, Message, Start, End, Option) ->
    send_interval(Register, Name, Spec, self(), Message, Start, End, Option).

?DOC("""
Sends a message to a process repeatedly based on a crontab schedule.

### Parameters
* `Register` - Process name where job will be registered
* `JobName` - Unique identifier for the job
* `Spec` - Crontab expression defining execution schedule
* `Pid` - Destination process ID or registered name
* `Message` - Term to send on each trigger
* `Start` - Start time `{Hour,Min,Sec}` or `unlimited`
* `End` - End time `{Hour,Min,Sec}` or `unlimited`
* `Option` - Same options as `add/7`

Returns: `{ok, reference()}` | `{error, parse_error(), term()}`
""").
-spec send_interval(
    register(), name(), crontab_spec(), pid(), term(), start_at(), end_at(), options()
) ->
    ecron_result().
send_interval(Register, JobName, Spec, Pid, Message, Start, End, Option) ->
    add(Register, JobName, Spec, {erlang, send, [Pid, Message]}, Start, End, Option).

?DOC("""
Same as [`delete(ecron_local, Name)`](`delete/2`).
""").
-spec delete(name()) -> ok.
delete(JobName) -> delete(?LocalJob, JobName).

?DOC("""
Deletes an existing job. If the job does not exist, it will be ignored.

Parameters:
* `Register` - Process name where job is registered
* `JobName` - Name of job to delete

Returns: `ok`
""").
-spec delete(register(), name()) -> ok.
delete(Register, JobName) ->
    ok = gen_server:call(Register, {delete, JobName}, infinity).

?DOC("""
Same as [`deactivate(ecron_local, Name)`](`deactivate/2`).
""").
-spec deactivate(name()) -> ok | {error, not_found}.
deactivate(JobName) -> deactivate(?LocalJob, JobName).

?DOC("""
Deactivates an existing job temporarily. The job can be reactivated using activate/2.

Parameters:
* `Register` - Process name where job is registered  
* `JobName` - Name of job to deactivate

Returns: `ok` | `{error, not_found}`
""").
-spec deactivate(register(), name()) -> ok | {error, not_found}.
deactivate(Register, JobName) ->
    case gen_server:call(Register, {deactivate, JobName}, infinity) of
        ok -> ok;
        {error, not_found} -> {error, not_found}
    end.

?DOC("""
Same as [`activate(ecron_local, Name)`](`activate/2`).
""").
-spec activate(name()) -> ok | {error, not_found}.
activate(JobName) -> activate(?LocalJob, JobName).

?DOC("""
Activates a previously deactivated job. The job will resume from the current time.

Parameters:
* `Register` - Process name where job is registered
* `JobName` - Name of job to activate

Returns: `ok` | `{error, not_found}`
""").
-spec activate(register(), name()) -> ok | {error, not_found}.
activate(Register, JobName) ->
    case gen_server:call(Register, {activate, JobName}, infinity) of
        ok -> ok;
        {error, not_found} -> {error, not_found}
    end.

?DOC("""
Retrieves statistics for local registered jobs.

Returns: List of statistics for all jobs in local registries.
Each statistic entry contains the same information as `statistic/2`.
""").
-spec statistic(register() | name()) -> [statistic()].
statistic(Register) ->
    case is_atom(Register) andalso undefined =/= erlang:whereis(Register) of
        false ->
            [job_to_statistic(Job) || Job <- ets:lookup(?LocalJob, Register)];
        true ->
            ets:foldl(fun(Job, Acc) -> [job_to_statistic(Job) | Acc] end, [], Register)
    end.

?DOC("""
Retrieves statistics for a specific job.

Parameters:
* `Register` - Process name where job is registered
* `JobName` - Name of job to get statistics for

Returns: `{ok, statistic()}` | `{error, not_found}`

Where statistic() contains:
- Job configuration
- Execution counts (success/failure)  
- Latest results
- Run times
- Next scheduled runs
""").
-spec statistic(register(), name()) -> {ok, statistic()} | {error, not_found}.
statistic(Register, JobName) ->
    case ets:lookup(Register, JobName) of
        [Job] ->
            {ok, job_to_statistic(Job)};
        [] ->
            try
                gen_server:call({global, ?GlobalJob}, {statistic, JobName})
            catch
                _:_ ->
                    {error, not_found}
            end
    end.

?DOC("""
Retrieves statistics for both local and global registered jobs.

Returns: List of statistics for all jobs in both local and global registries.
Each statistic entry contains the same information as statistic/2.
""").
-spec statistic() -> [statistic()].
statistic() ->
    Local = ets:foldl(
        fun(Job, Acc) when is_list(Acc) -> [job_to_statistic(Job) | Acc] end, [], ?LocalJob
    ),
    Global =
        try
            gen_server:call({global, ?GlobalJob}, statistic)
        catch
            _:_ ->
                []
        end,
    is_list(Global) orelse throw({error, invalid_statistic, Global}),
    is_list(Local) orelse throw({error, invalid_statistic, Local}),
    lists:append(Local, Global).

?DOC("""
Reloads all tasks manually. Useful when system time has changed significantly.

This will:
- Recalculate next execution times for all jobs
- Reset internal timers
- Apply to both local and global jobs

Returns: `ok`
""").
-spec reload() -> ok.
reload() ->
    gen_server:cast(?LocalJob, reload),
    gen_server:cast({global, ?GlobalJob}, reload).

?DOC("""
Parses a crontab specification and returns the next N trigger times.
Useful for debugging and validating crontab expressions.

Parameters:
* `Spec` - Crontab expression to parse
* `Num` - Number of future trigger times to calculate

Returns: `{ok, #{type => cron|every, crontab => parsed_spec, next => [rfc3339_string()]}}` |
`{error, parse_error(), term()}`
""").
-spec parse_spec(crontab_spec(), pos_integer()) ->
    {ok, #{type => cron | every, crontab => crontab_spec(), next => [rfc3339_string()]}}
    | {error, atom(), term()}.
parse_spec(Spec, Num) when is_integer(Num) andalso Num > 0 ->
    parse_spec2(ecron_spec:parse_spec(Spec), Num).

parse_spec2({ok, Type, JobSpec}, Num) ->
    Job = #{type => Type, crontab => JobSpec},
    Next = predict_datetime(Job, Num),
    {ok, Job#{next => Next}};
parse_spec2({error, _Field, _Value} = Error, _Num) ->
    Error.

get_next_schedule_time(Name) -> gen_server:call(?LocalJob, {next_schedule_time, Name}, infinity).

%% @private
clear() -> gen_server:call(?LocalJob, clear, infinity).

predict_datetime(Job, Num) ->
    TZ = get_time_zone(),
    Now = current_millisecond(),
    predict_datetime(activate, Job, {0, 0, 0}, {23, 59, 59}, Num, TZ, Now).

%%%===================================================================
%%% CallBack
%%%===================================================================
%% @private
start_link({_, JobTab} = Name, JobSpec) ->
    case ecron_spec:parse_crontab(JobSpec, []) of
        {ok, Jobs} ->
            new_job_tab(JobTab, ets:info(JobTab)),
            gen_server:start_link(Name, ?MODULE, [JobTab, Jobs], []);
        {error, Reason} ->
            {error, Reason}
    end;
start_link(JobTab, JobSpec) when is_atom(JobTab) ->
    start_link({local, JobTab}, JobSpec).
%% @private
init([JobTab, Jobs]) ->
    erlang:process_flag(trap_exit, true),
    TimerTab = ets:new(ecron_timer, [ordered_set, protected, {keypos, #timer.key}]),
    TimeZone = get_time_zone(),
    MaxTimeout = application:get_env(?Ecron, adjusting_time_second, 7 * 24 * 3600),
    [ets:insert_new(JobTab, Job) || Job <- Jobs],
    [
        add_job(JobTab, TimerTab, Job, TimeZone, Opts, true)
     || #job{job = Job, opts = Opts, status = activate} <- ets:tab2list(JobTab)
    ],

    State = #state{
        timer_tab = TimerTab,
        job_tab = JobTab,
        max_timeout = MaxTimeout * 1000,
        time_zone = TimeZone
    },
    {ok, State, next_timeout(State)}.
%% @private
handle_call({add, Job, Opts}, _From, State) ->
    #state{timer_tab = TimerTab, job_tab = JobTab, time_zone = TimeZone} = State,
    Reply = add_job(JobTab, TimerTab, Job, TimeZone, Opts, false),
    {reply, Reply, State, tick(State)};
handle_call({delete, Name}, _From, State) ->
    #state{timer_tab = TimerTab, job_tab = JobTab} = State,
    delete_job(JobTab, TimerTab, Name),
    {reply, ok, State, next_timeout(State)};
handle_call({activate, Name}, _From, State) ->
    #state{job_tab = JobTab, time_zone = TimeZone, timer_tab = TimerTab} = State,
    Reply = activate_job(Name, JobTab, TimerTab, TimeZone),
    {reply, Reply, State, tick(State)};
handle_call({deactivate, Name}, _From, State) ->
    #state{timer_tab = TimerTab, job_tab = JobTab} = State,
    Reply = deactivate_job(Name, JobTab, TimerTab),
    {reply, Reply, State, next_timeout(State)};
handle_call({statistic, Name}, _From, State) ->
    Reply = job_to_statistic(Name, State),
    {reply, Reply, State, next_timeout(State)};
handle_call(statistic, _From, State = #state{timer_tab = TimerTab}) ->
    Fun = fun(#timer{name = Name}, Acc) ->
        {ok, Item} = job_to_statistic(Name, State),
        [Item | Acc]
    end,
    Reply = ets:foldl(Fun, [], TimerTab),
    {reply, Reply, State, next_timeout(State)};
handle_call({next_schedule_time, Name}, _From, State = #state{timer_tab = TimerTab}) ->
    Reply = get_next_schedule_time(TimerTab, Name),
    {reply, Reply, State, next_timeout(State)};
handle_call(clear, _From, State = #state{timer_tab = TimerTab, job_tab = JobTab}) ->
    ets:delete_all_objects(TimerTab),
    ets:delete_all_objects(JobTab),
    {reply, ok, State, next_timeout(State)};
handle_call(_Unknown, _From, State) ->
    {noreply, State, next_timeout(State)}.
%% @private
handle_info(timeout, State) ->
    {noreply, State, tick(State)};
handle_info({'EXIT', Pid, _Reason}, State = #state{timer_tab = TimerTab, job_tab = JobTab}) ->
    delete_pid(Pid, TimerTab, JobTab),
    {noreply, State, next_timeout(State)};
handle_info(_Unknown, State) ->
    {noreply, State, next_timeout(State)}.
%% @private
handle_cast(_Unknown, State) ->
    {noreply, State, next_timeout(State)}.

%%%===================================================================
%%% First Internal Functions
%%%===================================================================
new_job_tab(JobTab, undefined) ->
    ets:new(JobTab, [named_table, set, public, {keypos, #job.name}]);
new_job_tab(_JobTab, _Info) ->
    ok.

add_job(JobTab, TimerTab, Job, TimeZone, Opts, ForceUpdate) ->
    #{name := Name, mfa := MFA} = Job,
    PidOrUndef = link_send_pid(MFA),
    JobRec = #job{status = activate, name = Name, job = Job, opts = Opts, link = PidOrUndef},
    IsNew = ets:insert_new(JobTab, JobRec),
    case IsNew orelse ForceUpdate of
        true ->
            InitTimer = #timer{
                singleton = proplists:get_value(singleton, Opts),
                max_count = proplists:get_value(max_count, Opts),
                name = Name,
                mfa = MFA,
                link = PidOrUndef
            },
            Now = current_millisecond(),
            telemetry:execute(?Activate, #{action_ms => Now}, #{name => Name, mfa => MFA}),
            update_timer(Now, InitTimer, Job, TimerTab, TimeZone);
        false ->
            {error, already_exist}
    end.

activate_job(Name, JobTab, TimerTab, TimeZone) ->
    case ets:lookup(JobTab, Name) of
        [] ->
            {error, not_found};
        [#job{job = Job, opts = Opts}] ->
            delete_job(JobTab, TimerTab, Name),
            {ok, Name} = add_job(JobTab, TimerTab, Job, TimeZone, Opts, false),
            ok
    end.

deactivate_job(Name, JobTab, TimerTab) ->
    ets:select_delete(TimerTab, ?MatchNameSpec(Name)),
    case ets:update_element(JobTab, Name, {#job.status, deactivate}) of
        true ->
            telemetry:execute(?Deactivate, #{action_ms => current_millisecond()}, #{name => Name}),
            ok;
        false ->
            {error, not_found}
    end.

delete_job(JobTab, TimerTab, Name) ->
    ets:select_delete(TimerTab, ?MatchNameSpec(Name)),
    case ets:lookup(JobTab, Name) of
        [] ->
            ok;
        [#job{link = Link}] ->
            telemetry:execute(?Delete, #{action_ms => current_millisecond()}, #{name => Name}),
            unlink_pid(Link),
            ets:delete(JobTab, Name)
    end.

%%%===================================================================
%%% Second Internal Functions
%%%===================================================================
update_timer(Now, InitTimer, Job, TimeTab, TimeZone) ->
    #{
        name := Name,
        crontab := Spec,
        type := Type,
        start_time := Start,
        end_time := End
    } = Job,
    NextSec = next_schedule_millisecond(Type, Spec, TimeZone, Now, Start, End),
    Timer = InitTimer#timer{
        key = {NextSec, Name},
        type = Type,
        spec = Spec,
        start_at = Start,
        end_at = End
    },
    ets:insert(TimeTab, Timer),
    {ok, Name}.

next_schedule_millisecond(every, Sec, TimeZone, Now, Start, End) ->
    Next = Now + Sec * 1000,
    case Start =:= {0, 0, 0} andalso End =:= {23, 59, 59} of
        true ->
            Next;
        false ->
            {_, {NowHour, NowMin, NowSec}} = millisecond_to_datetime(TimeZone, Next),
            {StartHour, StartMin, StartSec} = Start,
            {EndHour, EndMin, EndSec} = End,
            NowTime = NowHour * 3600 + NowMin * 60 + NowSec,
            StartTime = StartHour * 3600 + StartMin * 60 + StartSec,
            EndTime = EndHour * 3600 + EndMin * 60 + EndSec,
            case NowTime >= StartTime andalso NowTime =< EndTime of
                true -> Next;
                false -> next_schedule_millisecond(every, Sec, TimeZone, Next, Start, End)
            end
    end;
next_schedule_millisecond(cron, Spec, TimeZone, Now, Start, End) ->
    ForwardDateTime = millisecond_to_datetime(TimeZone, Now + 1000),
    DefaultMin = #{
        second => 0,
        minute => 0,
        hour => 0,
        day_of_month => 1,
        month => 1,
        day_of_week => 0
    },
    MinSpec = spec_min(maps:to_list(Spec), DefaultMin),
    next_schedule_millisecond2(Spec, MinSpec, ForwardDateTime, Start, End, TimeZone).

next_schedule_millisecond2(Spec, MinSpec, ForwardDateTime, Start, End, TimeZone) ->
    NextDateTime =
        {_, {NH, NM, NS}} =
        next_schedule_datetime(Spec, MinSpec, ForwardDateTime, Start, End),
    Next = NH * 3600 + NM * 60 + NS,
    {SHour, SMin, SSec} = Start,
    {EHour, EMin, ESec} = End,
    case
        Next >= SHour * 3600 + SMin * 60 + SSec andalso
            Next =< EHour * 3600 + EMin * 60 + ESec
    of
        true ->
            datetime_to_millisecond(TimeZone, NextDateTime);
        false ->
            next_schedule_millisecond2(Spec, MinSpec, NextDateTime, Start, End, TimeZone)
    end.

next_schedule_datetime(DateSpec, Min, DateTime, Start, End) ->
    #{
        second := SecondSpec,
        minute := MinuteSpec,
        hour := HourSpec,
        day_of_month := DayOfMonthSpec,
        month := MonthSpec,
        day_of_week := DayOfWeekSpec
    } = DateSpec,
    {SHour, SMin, SSec} = Start,
    {EHour, EMin, ESec} = End,
    {{Year, Month, Day}, {Hour, Minute, Sec}} = DateTime,
    case valid_datetime(MonthSpec, Month) of
        false ->
            forward_month(DateTime, Min, DateSpec);
        true ->
            case valid_day(Year, Month, Day, DayOfMonthSpec, DayOfWeekSpec) of
                false ->
                    forward_day(DateTime, Min, DateSpec);
                true ->
                    case valid_datetime(HourSpec, Hour) of
                        false ->
                            forward_hour(DateTime, Min, DateSpec);
                        true when Hour < SHour ->
                            Begin = {{Year, Month, Day}, {SHour - 1, SMin, SSec}},
                            forward_hour(Begin, Min, DateSpec);
                        true when Hour > EHour ->
                            forward_day(DateTime, Min, DateSpec);
                        true ->
                            case valid_datetime(MinuteSpec, Minute) of
                                false ->
                                    forward_minute(DateTime, Min, DateSpec);
                                true when Hour =:= SHour andalso Minute < SMin ->
                                    Begin = {{Year, Month, Day}, {Hour, SMin - 1, SSec}},
                                    forward_minute(Begin, Min, DateSpec);
                                true when Hour =:= EHour andalso Minute > EMin ->
                                    forward_day(DateTime, Min, DateSpec);
                                true ->
                                    case valid_datetime(SecondSpec, Sec) of
                                        false ->
                                            forward_second(DateTime, Min, DateSpec);
                                        true when
                                            Hour =:= SHour andalso Minute =:= SMin andalso
                                                Sec < SSec
                                        ->
                                            Begin = {{Year, Month, Day}, {Hour, SMin, SSec - 1}},
                                            forward_second(Begin, Min, DateSpec);
                                        true when
                                            Hour =:= EHour andalso Minute =:= EMin andalso
                                                Sec > ESec
                                        ->
                                            forward_day(DateTime, Min, DateSpec);
                                        true ->
                                            DateTime
                                    end
                            end
                    end
            end
    end.

tick(State = #state{timer_tab = TimerTab}) ->
    tick_tick(ets:first(TimerTab), current_millisecond(), State).

current_millisecond() -> erlang:system_time(millisecond).

tick_tick('$end_of_table', _Cur, _State) ->
    infinity;
tick_tick({Due, _Name}, Cur, #state{max_timeout = MaxTimeout}) when Due > Cur ->
    min(Due - Cur, MaxTimeout);
tick_tick(Key = {Due, Name}, Cur, State) ->
    #state{time_zone = TZ, timer_tab = TimerTab, job_tab = JobTab} = State,
    [Cron] = ets:lookup(TimerTab, Key),
    #timer{singleton = Singleton, mfa = MFA, max_count = MaxCount, cur_count = CurCount} = Cron,
    ets:delete(TimerTab, Key),
    {Incr, CurPid} = maybe_spawn_worker(Cur - Due < 1000, Singleton, Name, MFA, JobTab),
    update_next_schedule(CurCount + Incr, MaxCount, Cron, Cur, Name, TZ, CurPid, TimerTab, JobTab),
    tick(State).

maybe_spawn_worker(true, _, Name, {erlang, send, Args}, JobTab) ->
    {1, spawn_mfa(JobTab, Name, {erlang, send, Args})};
maybe_spawn_worker(true, true, Name, MFA, JobTab) ->
    {1, spawn(?MODULE, spawn_mfa, [JobTab, Name, MFA])};
maybe_spawn_worker(true, false, Name, MFA, JobTab) ->
    spawn(?MODULE, spawn_mfa, [JobTab, Name, MFA]),
    {1, false};
maybe_spawn_worker(true, Pid, Name, MFA, JobTab) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            error_logger:error_msg(
                "Job ~p skipped - last execution is still running (pid:~p, fn:~p)."
                " Use {singleton,false} for concurrent runs~n",
                [Name, Pid, erlang:process_info(Pid, current_function)]
            ),
            {0, Pid};
        false ->
            {1, spawn(?MODULE, spawn_mfa, [JobTab, Name, MFA])}
    end;
maybe_spawn_worker(false, Singleton, _Name, _MFA, _JobTab) ->
    {0, Singleton}.

update_next_schedule(Max, Max, _Cron, _Cur, Name, _TZ, _CurPid, Tab, JobTab) ->
    delete_job(JobTab, Tab, Name);
update_next_schedule(Count, _Max, Cron, Cur, Name, TZ, CurPid, Tab, _JobTab) ->
    #timer{type = Type, start_at = Start, end_at = End, spec = Spec} = Cron,
    Next = next_schedule_millisecond(Type, Spec, TZ, Cur, Start, End),
    NextTimer = Cron#timer{key = {Next, Name}, singleton = CurPid, cur_count = Count},
    ets:insert(Tab, NextTimer).

%% @private
spawn_mfa(JobTab, Name, MFA) ->
    Start = erlang:monotonic_time(),
    {Event, OkInc, FailedInc, NewRes} =
        try
            case MFA of
                {erlang, send, [Pid, Message]} ->
                    erlang:send(Pid, Message),
                    {?Success, 1, 0, Message};
                {M, F, A} ->
                    {?Success, 1, 0, apply(M, F, A)};
                {F, A} ->
                    {?Success, 1, 0, apply(F, A)}
            end
        catch
            Error:Reason:Stacktrace ->
                {?Failure, 0, 1, {Error, Reason, Stacktrace}}
        end,
    End = erlang:monotonic_time(),
    Cost = erlang:convert_time_unit(End - Start, native, microsecond),
    telemetry:execute(Event, #{run_microsecond => Cost, run_result => NewRes}, #{
        name => Name,
        mfa => MFA
    }),
    case ets:lookup(JobTab, Name) of
        [] ->
            ok;
        [Job] ->
            #job{ok = Ok, failed = Failed, run_microsecond = RunMs, result = Results} = Job,
            Elements = [
                {#job.ok, Ok + OkInc},
                {#job.failed, Failed + FailedInc},
                {#job.run_microsecond, lists:sublist([Cost | RunMs], ?MAX_SIZE)},
                {#job.result, lists:sublist([NewRes | Results], ?MAX_SIZE)}
            ],
            ets:update_element(JobTab, Name, Elements)
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
        true ->
            forward_hour(DateTime, Min, Spec);
        false ->
            #{second := SecondM} = Min,
            {{Year, Month, Day}, {Hour, NewMinute, SecondM}}
    end.

forward_hour(DateTime, Min, Spec) ->
    {{Year, Month, Day}, {Hour, _Minute, _Second}} = DateTime,
    NewHour = nearest(hour, Hour, 23, Spec),
    case Hour >= NewHour of
        true ->
            forward_day(DateTime, Min, Spec);
        false ->
            #{minute := MinuteM, second := SecondM} = Min,
            {{Year, Month, Day}, {NewHour, MinuteM, SecondM}}
    end.

forward_day(DateTime, Min, Spec) ->
    {{Year, Month, _Day}, _} = DateTime,
    LastDay = calendar:last_day_of_the_month(Year, Month),
    forward_day(DateTime, Min, LastDay, Spec).

forward_day(DateTime, Min, LastDay, Spec) ->
    {{Year, Month, Day}, {_Hour, _Minute, _Second}} = DateTime,
    case Day + 1 of
        NewDay when NewDay > LastDay ->
            forward_month(DateTime, Min, Spec);
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
            forward_day(NewDateTime, Min, Spec);
        true ->
            NewDateTime
    end.

datetime_to_millisecond(local, DateTime) ->
    UtcTime = erlang:localtime_to_universaltime(DateTime),
    datetime_to_millisecond(utc, UtcTime);
datetime_to_millisecond(utc, DateTime) ->
    (calendar:datetime_to_gregorian_seconds(DateTime) - ?SECONDS_FROM_0_TO_1970) * 1000.

millisecond_to_datetime(local, Ms) -> calendar:system_time_to_local_time(Ms, millisecond);
millisecond_to_datetime(utc, Ms) -> calendar:system_time_to_universal_time(Ms, millisecond).

nearest(Type, Current, Max, Spec) ->
    Values = maps:get(Type, Spec),
    nearest_1(Values, Values, Max, Current + 1).

nearest_1('*', '*', MaxLimit, Next) when Next > MaxLimit ->
    1;
nearest_1('*', '*', _MaxLimit, Next) ->
    Next;
nearest_1([], [{Min, _} | _], _Max, _Next) ->
    Min;
nearest_1([], [Min | _], _Max, _Next) ->
    Min;
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

valid_day(_Year, _Month, _Day, '*', '*') ->
    true;
valid_day(_Year, _Month, Day, DayOfMonthSpec, '*') ->
    valid_datetime(DayOfMonthSpec, Day);
valid_day(Year, Month, Day, '*', DayOfWeekSpec) ->
    DayOfWeek = day_of_week(Year, Month, Day),
    valid_datetime(DayOfWeekSpec, DayOfWeek);
valid_day(Year, Month, Day, DayOfMonthSpec, DayOfWeekSpec) ->
    case valid_datetime(DayOfMonthSpec, Day) of
        false ->
            DayOfWeek = day_of_week(Year, Month, Day),
            valid_datetime(DayOfWeekSpec, DayOfWeek);
        true ->
            true
    end.

spec_min([], Acc) ->
    Acc;
spec_min([{Key, Value} | Rest], Acc) ->
    NewAcc =
        case Value of
            '*' -> Acc;
            [{Min, _} | _] -> Acc#{Key => Min};
            [Min | _] -> Acc#{Key => Min}
        end,
    spec_min(Rest, NewAcc).

next_timeout(#state{timer_tab = TimerTab, max_timeout = MaxTimeout} = State) ->
    case ets:first(TimerTab) of
        '$end_of_table' ->
            infinity;
        {Due, _} when is_integer(Due) ->
            Now = current_millisecond(),
            case Due =< Now of
                true -> tick(State);
                false -> min(Due - Now, MaxTimeout)
            end
    end.

to_rfc3339(Next) -> calendar:system_time_to_rfc3339(Next div 1000, [{unit, second}]).

predict_datetime(deactivate, _, _, _, _, _, _) ->
    [];
predict_datetime(activate, #{type := every, crontab := Sec} = Job, Start, End, Num, TimeZone, NowT) ->
    Now =
        case maps:find(name, Job) of
            error -> NowT;
            _ -> NowT - Sec * 1000
        end,
    predict_datetime2(Job, TimeZone, Now, Start, End, Num, []);
predict_datetime(activate, Job, Start, End, Num, TimeZone, Now) ->
    predict_datetime2(Job, TimeZone, Now, Start, End, Num, []).

predict_datetime2(_Job, _TimeZone, _Now, _Start, _End, 0, Acc) ->
    lists:reverse(Acc);
predict_datetime2(Job, TimeZone, Now, Start, End, Num, Acc) ->
    #{type := Type, crontab := Spec} = Job,
    Next = next_schedule_millisecond(Type, Spec, TimeZone, Now, Start, End),
    NewAcc = [to_rfc3339(Next) | Acc],
    predict_datetime2(Job, TimeZone, Next, Start, End, Num - 1, NewAcc).

get_next_schedule_time(Timer, Name) ->
    %% P = ets:fun2ms(fun(#timer{name = N, key = {Time, _}}) when N =:= Name -> Time end),
    P = [{#timer{key = {'$1', '_'}, name = '$2', _ = '_'}, [{'=:=', '$2', {const, Name}}], ['$1']}],
    case ets:select(Timer, P) of
        [T] -> T;
        [] -> current_millisecond()
    end.

get_time_zone() -> application:get_env(?Ecron, time_zone, local).

delete_pid(Pid, TimerTab, JobTab) ->
    TimerMatch = [{#timer{link = '$1', _ = '_'}, [], [{'=:=', '$1', {const, Pid}}]}],
    JobMatch = [{#job{link = '$1', _ = '_'}, [], [{'=:=', '$1', {const, Pid}}]}],
    ets:select_delete(TimerTab, TimerMatch),
    ets:select_delete(JobTab, JobMatch).

link_send_pid({erlang, send, [PidOrName, _Message]}) ->
    Pid = get_pid(PidOrName),
    is_pid(Pid) andalso (catch link(Pid)),
    Pid;
link_send_pid(_MFA) ->
    undefined.

unlink_pid(Pid) when is_pid(Pid) -> catch unlink(Pid);
unlink_pid(_) -> ok.

get_pid(Pid) when is_pid(Pid) -> Pid;
get_pid(Name) when is_atom(Name) -> whereis(Name).

job_to_statistic(Job = #job{name = Name}) ->
    TZ = get_time_zone(),
    Next = get_next_schedule_time(Name),
    job_to_statistic(Job, TZ, Next - 1000).

job_to_statistic(Name, State) ->
    #state{timer_tab = Timer, job_tab = JobTab, time_zone = TZ} = State,
    case ets:lookup(JobTab, Name) of
        [Job] ->
            Next = get_next_schedule_time(Timer, Name),
            {ok, job_to_statistic(Job, TZ, Next)};
        [] ->
            {error, not_found}
    end.

job_to_statistic(Job, TimeZone, Now) ->
    #job{
        job = JobSpec,
        status = Status,
        opts = Opts,
        ok = Ok,
        failed = Failed,
        result = Res,
        run_microsecond = RunMs
    } = Job,
    #{start_time := StartTime, end_time := EndTime} = JobSpec,
    JobSpec#{
        status => Status,
        ok => Ok,
        failed => Failed,
        opts => Opts,
        next => predict_datetime(Status, JobSpec, StartTime, EndTime, ?MAX_SIZE, TimeZone, Now),
        start_time => StartTime,
        end_time => EndTime,
        node => node(),
        results => Res,
        run_microsecond => RunMs
    }.

day_of_week(Y, M, D) ->
    case calendar:day_of_the_week(Y, M, D) of
        7 -> 0;
        D1 -> D1
    end.

%% For PropEr Test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

maybe_spawn_woker_test() ->
    ?assertEqual(
        {0, self()}, maybe_spawn_worker(false, self(), test_name, {erlang, datetime, []}, job_tab)
    ).
-endif.
