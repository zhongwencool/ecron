-module(ecron).

-include("ecron.hrl").

?MODULEDOC("""
`ecron` enables dynamic scheduling of tasks using crontab-style expressions. 
It provides a flexible API for managing recurring jobs, one-time tasks, 
and time-based message delivery.

""").
%% API Function
-export([start_link/1, start_link/2]).
-export([create/3, create/4]).
-export([send_after/3]).
-export([send_interval/2, send_interval/3, send_interval/4]).
-export([delete/1, delete/2]).
-export([deactivate/1, activate/1, deactivate/2, activate/2]).
-export([statistic/0, statistic/1, statistic/2]).
-export([predict_datetime/2, predict_datetime/4]).
-export([reload/0, reload/1]).
-export([parse_spec/2]).

%% Callback Function
-export([handle_call/3, handle_info/2, init/1, handle_cast/2]).
-export([spawn_mfa/3, spawn_mfa/4, clear/0]).

%% Deprecated Function
-export([add/3, add/4, add/6, add/7]).
-export([add_with_time/5, add_with_time/6]).
-export([add_with_count/3, add_with_count/5]).
-export([send_interval/5, send_interval/7, send_interval/8]).
-deprecated([
    {add, 3, "use create/3 instead"},
    {add, 4, "use create/4 instead"},
    {add, 6, "use create/4 instead"},
    {add, 7, "use create/4 instead"},
    {add_with_time, 5, "use create/4 instead"},
    {add_with_time, 6, "use create/4 instead"},
    {add_with_count, 3, "use create/4 instead"},
    {add_with_count, 5, "use create/4 instead"},
    {send_interval, 5, "use send_interval/4 instead"},
    {send_interval, 7, "use send_interval/4 instead"},
    {send_interval, 8, "use send_interval/4 instead"}
]).

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
    start_time = {0, 0, 0},
    end_time = {23, 59, 59},
    max_count = unlimited,
    max_runtime_ms = unlimited
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
-type rfc3339_string() :: string() | binary().

-type mfargs() :: {M :: module(), F :: atom(), A :: [term()]}.

-type statistic() :: #{
    name => name(),
    node => node(),
    type => cron | every,
    crontab => crontab(),
    status => deactivate | activate,
    crashed => non_neg_integer(),
    ok => non_neg_integer(),
    aborted => non_neg_integer(),
    opts => option_list(),
    results => [term()],
    run_microsecond => [pos_integer()],
    start_time => rfc3339_string() | unlimited,
    end_time => rfc3339_string() | unlimited,
    next => [calendar:datetime()]
}.

-type parse_error() ::
    invalid_time
    | invalid_opts
    | invalid_spec
    | month
    | day_of_month
    | day_of_week
    | hour
    | minute
    | second.

-type start_at() :: unlimited | calendar:time().
-type end_at() :: unlimited | calendar:time().
-type option_list() :: [
    {singleton, boolean()}
    | {max_count, pos_integer() | unlimited}
    | {max_runtime_ms, pos_integer() | unlimited}
].

-type options() :: #{
    singleton => boolean(),
    max_count => pos_integer() | unlimited,
    max_runtime_ms => pos_integer() | unlimited,
    start_time => start_at(),
    end_time => end_at(),
    register => register()
}.
-type ecron_result() :: {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.

?DOC("""
Same as [`create(JobName, Spec, MFA, #{})`](`create/4`).
## Examples
<!-- tabs-open -->

### Erlang
```erlang
UniqueJobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, UniqueJobName} = ecron:create(UniqueJobName, "0 4 * * *", MFA),
ecron:statistic(UniqueJobName).
```

### Elixir
```elixir
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.create(job_name, "0 4 * * *", mfa)
:ecron.statistic(job_name)
```

<!-- tabs-close -->

""").
-spec create(name(), crontab_spec(), mfargs()) -> ecron_result().
create(JobName, Spec, MFA) ->
    create(JobName, Spec, MFA, #{}).
-if(?OTP_RELEASE >= 27).
?DOC("""
Adds a new crontab job with specified parameters. 
Jobs exceeding their limits are automatically removed.

Parameters:
* `JobName` - Unique identifier for the job. Returns `{error, already_exist}` if duplicate
* `Spec` - [Crontab expression](readme.html#cron-expression-guide) defining execution schedule
* `MFA` - `{Module, Function, Args}` to execute when triggered
* `Opts` - Options map:
   * register: `atom()` - The process name where the job will be registered (default: `ecron_local`)
   * start_time: `{Hour,Min,Sec}|unlimited` - Start time for job execution (default: `unlimited`)
   * end_time: `{Hour,Min,Sec}|unlimited` - End time for job execution (default: `unlimited`) 
   * singleton: `boolean()` - If true, prevents concurrent execution (default: `false`)
   * max_count: `pos_integer()|unlimited` - Maximum number of executions allowed (default: `unlimited`). It will be automatically removed When a job reaches its max_count limit.
   * max_runtime_ms: `pos_integer()|unlimited` - Maximum runtime in milliseconds per execution (default: `unlimited`)

Returns: `{ok, JobName}` | `{error, already_exist}` | `{error, parse_error(), term()}`

# Examples
<!-- tabs-open -->

### Erlang
```erlang
Register = my_job_register,
{ok, _Pid} = ecron:start_link(Register, []),
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, JobName} = ecron:create(JobName, "0 4 * * *", MFA, #{
    start_time => {1, 0, 0}, 
    end_time => unlimited, 
    singleton => false,
    max_count => unlimited,
    max_runtime_ms => 1000,
    register => Register
}),
ecron:statistic(Register, JobName).
```

### Elixir
```elixir
register = :my_job_register
{:ok, _Pid} = :ecron.start_link(register, [])
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.create(job_name, "0 4 * * *", mfa, %{
    :start_time => {1, 0, 0}, 
    :end_time => :unlimited, 
    :singleton => false,
    :max_count => :unlimited,
    :max_runtime_ms => 1000,
    :register => register
})
:ecron.statistic(register, job_name)
```

<!-- tabs-close -->
> #### Singleton {: .info}
> When a job's singleton option is set to true, the system checks 
> if there is already an instance of the job running before starting a new execution. 
> If an instance is already running (old pid is alive), the new execution will be skipped.

> #### TimeRange {: .info}
> The start time must be less than the maximum value in the spec, and the end time must be greater than the minimum value in the spec.
>
> For example: With spec `0 1-12/1 * * *` the max value is 12 and min value is 1,
> so start time must be less than {12,0,0} and end time must be greater than {1,0,0}, with start < end.

If we can not find the next schedule time in the next 5 years, return 'cant find next schedule time in the next 5 years'.

```erlang
 ecron:create(invalid_job, "* 0,13 * * *", {io, format, [test]}, #{
       start_time => {1,0,0},
       end_time => {12,0,0}
   }).
{error,invalid_time,
       #{reason => "cant find next schedule time in the next 5 years",
         spec => "* 0,13 * * *",
         start_time => {1,0,0},
         end_time => {12,0,0}}}
```

""").
-endif.
-spec create(name(), crontab_spec(), mfargs(), options()) -> ecron_result().
create(JobName, Spec, MFA, Opts = #{}) ->
    Register = maps:get(register, Opts, ?LocalJob),
    StartAt = maps:get(start_time, Opts, unlimited),
    EndAt = maps:get(end_time, Opts, unlimited),
    RunCount = maps:get(max_count, Opts, unlimited),
    MaxRuntimeMs = maps:get(max_runtime_ms, Opts, unlimited),
    Singleton = maps:get(singleton, Opts, false),
    OptsList = [
        {max_count, RunCount},
        {max_runtime_ms, MaxRuntimeMs},
        {singleton, Singleton}
    ],
    create(Register, JobName, Spec, MFA, StartAt, EndAt, OptsList).

?DOC("""
Same as [`add(ecron_local, JobName, Spec, MFA)`](`add/4`).
`ecron_local` is the default register name, always exists.
## Examples
<!-- tabs-open -->

### Erlang
```erlang
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, JobName} = ecron:add(JobName, "0 4 * * *", MFA),
ecron:statistic(JobName).
```

### Elixir
```elixir
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.add(job_name, "0 4 * * *", mfa)
:ecron.statistic(job_name)
```

<!-- tabs-close -->

""").
-spec add(name(), crontab_spec(), mfargs()) -> ecron_result().
add(JobName, Spec, MFA) ->
    create(JobName, Spec, MFA, #{register => ?LocalJob}).

-if(?OTP_RELEASE >= 27).
?DOC("""
Same as [`add(Register, JobName, Spec, MFA, unlimited, unlimited, [])`](`add/7`).
## Examples
<!-- tabs-open -->

### Erlang
```erlang
Register = my_cronjob_register,
{ok, _Pid} = ecron:start_link(Register, []),
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, JobName} = ecron:add(Register, JobName, "0 4 * * *", MFA),
ecron:statistic(Register, JobName).
```

### Elixir
```elixir
register = :my_cronjob_register
{:ok, _Pid} = :ecron.start_link(register, [])
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.add(register, job_name, "0 4 * * *", mfa)
:ecron.statistic(register, job_name)
```
<!-- tabs-close -->

""").
-endif.
-spec add(register(), name(), crontab_spec(), mfargs()) -> ecron_result().
add(Register, JobName, Spec, MFA) ->
    create(Register, JobName, Spec, MFA, unlimited, unlimited, []).

?DOC("""
Same as [`add_with_count(ecron_local, make_ref(), Spec, MFA, RunCount)`](`add_with_count/5`).
`ecron_local` is the default register name, always exists.
## Examples
The job will be auto deleted after running 10 times.
<!-- tabs-open -->

### Erlang
```erlang
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, JobName} = ecron:add_with_count(JobName, "0 4 * * *", MFA, 10),
ecron:statistic(JobName).
```

### Elixir
```elixir
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.add_with_count(job_name, "0 4 * * *", mfa, 10)
:ecron.statistic(job_name)
```

<!-- tabs-close -->

""").
-spec add_with_count(crontab_spec(), mfargs(), pos_integer()) -> ecron_result().
add_with_count(Spec, MFA, RunCount) when is_integer(RunCount) ->
    create(make_ref(), Spec, MFA, #{register => ?LocalJob, max_count => RunCount}).

?DOC("""
Same as [`add(Register, JobName, Spec, MFA, unlimited, unlimited, [{max_count, RunCount}])`](`add/7`).
## Examples
The job will be auto deleted after running 10 times.
<!-- tabs-open -->

### Erlang
```erlang
Register = my_cronjob_register,
{ok, _Pid} = ecron:start_link(Register, []),
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, JobName} = ecron:add_with_count(Register, JobName, "0 4 * * *", MFA, 10),
ecron:statistic(Register, JobName).
```

### Elixir
```elixir
register = :my_cronjob_register
{:ok, _Pid} = :ecron.start_link(register, [])
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.add_with_count(register, job_name, "0 4 * * *", mfa, 10)
:ecron.statistic(register, job_name)
```

<!-- tabs-close -->

""").
-spec add_with_count(register(), name(), crontab_spec(), mfargs(), pos_integer()) ->
    ecron_result().
add_with_count(Register, JobName, Spec, MFA, RunCount) when is_integer(RunCount) ->
    create(Register, JobName, Spec, MFA, unlimited, unlimited, [{max_count, RunCount}]).

-if(?OTP_RELEASE >= 27).
?DOC("""
Add a job into default register with start and end time.
If you want to use the specified register, use [`add_with_time/6`](#add_with_time/6).
## Examples
The job will be auto skipped if the current time is not between 04:00 and 12:00 everyday.
<!-- tabs-open -->
### Erlang
```erlang
JobName = on_hour_job,
MFA = {io, format, ["Run at 04:00-12:00 on the hour.~n", []]},
{ok, JobName} = ecron:add_with_time(JobName, "0 1-12/1 * * *", MFA, {4, 0, 0}, {12, 0, 0}),
ecron:statistic(JobName).
```
### Elixir
```elixir
job_name = :on_hour_job
mfa = {IO, :puts, ["Run at 04:00-12:00 on the hour.\n"]}
{:ok, ^job_name} = :ecron.add_with_time(job_name, "0 1-12/1 * * *", mfa, {4, 0, 0}, {12, 0, 0})
:ecron.statistic(job_name)
```
<!-- tabs-close -->
    
> #### TimeRange {: .info}
> The start time must be less than the maximum value in the spec, and the end time must be greater than the minimum value in the spec.
>
> For example: With spec `0 1-12/1 * * *` the max value is 12 and min value is 1,
> so start time must be less than {12,0,0} and end time must be greater than {1,0,0}, with start < end.
    
If we can not find the next schedule time in the next 5 years, return `cant find next schedule time in the next 5 years`.
    
```erlang
ecron:add_with_time(invalid_job, "* 0,13 * * *", {io, format, ["test"]},{1,0,0},{12,0,0}).
{error,invalid_time,
        #{reason => "cant find next schedule time in the next 5 years",
            start => {1,0,0},
            stop => {12,0,0},
            spec => "* 0,13 * * *"}}
```
""").
-endif.
-spec add_with_time(name(), crontab_spec(), mfargs(), start_at(), end_at()) ->
    ecron_result().
add_with_time(JobName, Spec, MFA, Start, End) ->
    create(JobName, Spec, MFA, #{register => ?LocalJob, start_time => Start, end_time => End}).

-if(?OTP_RELEASE >= 27).
?DOC("""
Add a job into specified register with start and end time.
If you want to use the default register, use [`add_with_time/5`](#add_with_time/5).
## Examples
The job will be auto skipped if the current time is not between 04:00 and 12:00 everyday.
<!-- tabs-open -->

### Erlang
```erlang
Register = my_cronjob_register,
{ok, _Pid} = ecron:start_link(Register, []),
JobName = on_hour_job,
MFA = {io, format, ["Run at 04:00-12:00 on the hour.~n", []]},
{ok, JobName} = ecron:add_with_time(Register, JobName, "0 1-12/1 * * *", MFA, {4, 0, 0}, {12, 0, 0}),
ecron:statistic(Register, JobName).
```

### Elixir
```elixir
register = :my_cronjob_register
{:ok, _Pid} = :ecron.start_link(register, [])
job_name = :on_hour_job
mfa = {IO, :puts, ["Run at 04:00-12:00 on the hour.\n"]}
{:ok, ^job_name} = :ecron.add_with_time(register, job_name, "0 1-12/1 * * *", mfa, {4, 0, 0}, {12, 0, 0})
:ecron.statistic(register, job_name)
```
<!-- tabs-close -->

> #### TimeRange {: .info}
> The start time must be less than the maximum value in the spec, and the end time must be greater than the minimum value in the spec.
>
> For example: With spec `0 1-12/1 * * *` the max value is 12 and min value is 1,
> so start time must be less than {12,0,0} and end time must be greater than {1,0,0}, with start < end.

If we can not find the next schedule time in the next 5 years, return `cant find next schedule time in the next 5 years`.

```erlang
ecron:add_with_time(invalid_job, "* 0,13 * * *", {io, format, ["test"]},{1,0,0},{12,0,0}).
{error,invalid_time,
       #{reason => "cant find next schedule time in the next 5 years",
         start => {1,0,0},
         stop => {12,0,0},
         spec => "* 0,13 * * *"}}
```
""").
-endif.
-spec add_with_time(register(), name(), crontab_spec(), mfargs(), start_at(), end_at()) ->
    ecron_result().
add_with_time(Register, JobName, Spec, MFA, Start, End) ->
    create(Register, JobName, Spec, MFA, Start, End, []).

?DOC("""
Same as [`add(ecron_local, JobName, Spec, MFA, Start, End, Opts)`](`add/7`).
""").
-spec add(name(), crontab_spec(), mfargs(), start_at(), end_at(), option_list()) ->
    ecron_result().
add(JobName, Spec, MFA, Start, End, Opts) ->
    create(?LocalJob, JobName, Spec, MFA, Start, End, Opts).

?DOC("""
Adds a new crontab job with specified parameters. 
Jobs exceeding their limits are automatically removed.

Parameters:
* `Register` - The process name where the job will be registered
* `JobName` - Unique identifier for the job. Returns `{error, already_exist}` if duplicate
* `Spec` - [Crontab expression](readme.html#cron-expression-guide) defining execution schedule
* `MFA` - `{Module, Function, Args}` to execute when triggered
* `Start` - Start time `{Hour,Min,Sec}` or `unlimited` for immediate start
* `End` - End time `{Hour,Min,Sec}` or `unlimited` for no end
* `Opts` - Options list:
    * `{singleton, boolean()}` - If true (default), prevents concurrent execution
    * `{max_count, pos_integer() | unlimited}` - Maximum executions allowed
    * `{max_runtime_ms, pos_integer() | unlimited}` - Maximum runtime milliseconds allowed for each execution

Returns: `{ok, JobName}` | `{error, already_exist}` | `{error, parse_error(), term()}`

# Examples
<!-- tabs-open -->

### Erlang
```erlang
Register = my_cronjob_register,
{ok, _Pid} = ecron:start_link(Register, []),
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, JobName} = ecron:add(Register, JobName, "0 4 * * *", MFA, {1, 0, 0}, unlimited, [{singleton, true}]),
ecron:statistic(Register, JobName).
```

### Elixir
```elixir
register = :my_cronjob_register
{:ok, _Pid} = :ecron.start_link(register, [])
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
{:ok, ^job_name} = :ecron.add(register, job_name, "0 4 * * *", mfa, {1, 0, 0}, :unlimited, [singleton: true])
:ecron.statistic(register, job_name)
```

<!-- tabs-close -->
> #### Singleton {: .info}
> When a job's singleton option is set to true, the system checks 
> if there is already an instance of the job running before starting a new execution. 
> If an instance is already running（old pid is alive）, the new execution will be skipped.

""").
-spec add(register(), name(), crontab_spec(), mfargs(), start_at(), end_at(), option_list()) ->
    ecron_result().
add(Register, JobName, Spec, MFA, StartAt, EndAt, Opts) ->
    create(Register, JobName, Spec, MFA, StartAt, EndAt, Opts).

create(Register, JobName, Spec, MFA, StartAt, EndAt, Opts) ->
    case ecron_spec:parse_start_end_time(StartAt, EndAt) of
        {StartTime, EndTime} ->
            case ecron_spec:parse_spec(Spec) of
                {ok, Type, Crontab} ->
                    case ecron_spec:valid_time(Type, StartTime, EndTime, Crontab) of
                        ok ->
                            Job = #{
                                type => Type,
                                name => JobName,
                                crontab => Crontab,
                                mfa => MFA,
                                start_time => StartTime,
                                end_time => EndTime
                            },
                            case ecron_spec:parse_valid_opts(Opts) of
                                {ok, ValidOpts} ->
                                    case
                                        gen_server:call(Register, {add, Job, ValidOpts}, infinity)
                                    of
                                        {ok, Name} -> {ok, Name};
                                        {error, already_exist} -> {error, already_exist}
                                    end;
                                ErrOpts ->
                                    ErrOpts
                            end;
                        {error, Reason} ->
                            {error, invalid_time, #{
                                start_time => StartAt,
                                end_time => EndAt,
                                spec => Spec,
                                reason => Reason
                            }}
                    end;
                ErrParse ->
                    ErrParse
            end;
        false ->
            {error, invalid_time, #{start_time => StartAt, end_time => EndAt, spec => Spec}}
    end.

?DOC("""
Create a one-time timer that sends a message when the crontab spec is triggered.

Parameters:
* `Spec` - Crontab expression defining when to trigger
* `Dest` - Destination pid() or registered name to receive message
* `Message` - Term to send when timer triggers

Notes:
* Similar to `erlang:send_after/3` but uses crontab format
* Dest pid must be local
* Maximum time value is 4294967295 milliseconds
* Timer auto-cancels if destination process dies

Returns: `{ok, reference()}` | `{error, parse_error(), term()}`

# Examples
<!-- tabs-open -->

### Erlang
```erlang
ecron:send_after("*/3 * * * * *", self(), hello_world).
```

### Elixir
```elixir
:ecron.send_after("*/3 * * * * *", self(), :hello_world)
```
<!-- tabs-close -->

> #### Statistic {: .info}
> This is one-time timer, so it not seen in `statistic/0` result.    
>
> Use `erlang:cancel_timer/1` to cancel, not `ecron:delete/1`

""").
-spec send_after(crontab_spec(), pid() | atom(), term()) ->
    {ok, reference()} | {error, parse_error(), term()}.
send_after(Spec, Pid, Message) ->
    case ecron_spec:parse_spec(Spec) of
        {ok, Type, JobSpec} ->
            TimeZone = get_time_zone(),
            Now = current_millisecond(),
            {ok, Next} = next_schedule_millisecond(
                Type, JobSpec, TimeZone, Now, {0, 0, 0}, {23, 59, 59}, Now
            ),
            {ok, erlang:send_after(Next - Now, Pid, Message)};
        {error, _Field, _Value} = Error ->
            Error
    end.

?DOC("""
Sends a message to a process repeatedly based on a crontab schedule from the default registry. 
Same as [`send_interval(ecron_local, make_ref(), Spec, Pid, Message, unlimited, unlimited, [])`](`send_interval/7`).
""").
-spec send_interval(crontab_spec(), pid(), term()) -> ecron_result().
send_interval(Spec, Pid, Message) ->
    send_interval(Spec, Pid, Message, #{}).

?DOC("""
Sends a message to a process repeatedly based on a crontab schedule from the default registry. 
Same as [`send_interval(Spec, Pid, Message, #{register => ecron_local})`](`send_interval/4`).
""").
-spec send_interval(crontab_spec(), term()) -> ecron_result().
send_interval(Spec, Message) ->
    send_interval(Spec, self(), Message, #{}).

?DOC("""
Evaluates `Pid ! Message` repeatedly after crontab schedule milliseconds.

Parameters:
* `Spec` - Crontab expression defining when to trigger
* `Pid` - Destination process ID or registered name
* `Message` - Term to send on each trigger
* `Opts` - Same options as `create/4`

Returns: `{ok, reference()}` | `{error, parse_error(), term()}`

# Examples
<!-- tabs-open -->

### Erlang
```erlang
ecron:send_interval("*/3 * * * * *", self(), hello_world).
```

### Elixir
```elixir
:ecron.send_interval("*/3 * * * * *", self(), :hello_world)
```
<!-- tabs-close -->

> #### Statistic {: .info}
> This is repeatable timer, so it seen in `statistic/0` result.    
>
> Use `ecron:delete/1` to cancel, not `erlang:cancel_timer/1`

""").
-spec send_interval(crontab_spec(), pid(), term(), options()) -> ecron_result().
send_interval(Spec, Pid, Message, Opts) ->
    create(make_ref(), Spec, {erlang, send, [Pid, Message]}, Opts).

?DOC("""
Sends a message to a process repeatedly based on a crontab schedule from the specified registry. 
Same as [`send_interval(Register, Name, Spec, Pid, Message, unlimited, unlimited, [])`](`send_interval/7`).
""").
-spec send_interval(register(), name(), crontab_spec(), pid(), term()) -> ecron_result().
send_interval(Register, Name, Spec, Pid, Message) ->
    send_interval(Register, Name, Spec, Pid, Message, unlimited, unlimited, []).

?DOC("""
Same as [`send_interval(register(), name(), Spec, self(), Message, Start, End, Option)`](`send_interval/8`).
""").
-spec send_interval(
    register(), name(), crontab_spec(), term(), start_at(), end_at(), option_list()
) ->
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
    register(), name(), crontab_spec(), pid(), term(), start_at(), end_at(), option_list()
) ->
    ecron_result().
send_interval(Register, JobName, Spec, Pid, Message, Start, End, Option) ->
    create(Register, JobName, Spec, {erlang, send, [Pid, Message]}, Start, End, Option).

?DOC("""
Deletes an existing job from the default registry. 
If the job does not exist, it will be ignored.
Use [`delete/2`](#delete/2) to delete a job from a specified registry.

Returns: `ok`
""").
-spec delete(name()) -> ok.
delete(JobName) -> delete(?LocalJob, JobName).

?DOC("""
Deletes an existing job from the specified registry.
If the job does not exist, it will be ignored.
Use [`delete/1`](#delete/1) to delete a job from the default registry.

Parameters:
* `Register` - Process name where job is registered
* `JobName` - Name of job to delete

Returns: `ok`
""").
-spec delete(register(), name()) -> ok.
delete(Register, JobName) ->
    ok = gen_server:call(Register, {delete, JobName}, infinity).

?DOC("""
Deactivates an existing job temporarily from the default registry. 
The job can be reactivated using [`activate/1`](#activate/1).
""").
-spec deactivate(name()) -> ok | {error, not_found}.
deactivate(JobName) -> deactivate(?LocalJob, JobName).

?DOC("""
Deactivates an existing job temporarily from the specified registry. 
The job can be reactivated using [`activate/2`](#activate/2).

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
Activates a previously deactivated job from the default registry. 
The job can be deactivated using [`deactivate/1`](#deactivate/1).
""").
-spec activate(name()) -> ok | {error, not_found}.
activate(JobName) -> activate(?LocalJob, JobName).

?DOC("""
Activates a previously deactivated job from the specified registry. 
The job will resume from the current time.
The job can be deactivated using [`deactivate/2`](#deactivate/2).

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
Retrieves statistics for default registry.
Use [`statistic/2`](#statistic/2) to get statistics for a specific registry.

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
Retrieves statistics for a specific job from the specified registry.
Use [`statistic/1`](#statistic/1) to get statistics for the default registry.

Parameters:
* `Register` - Process name where job is registered
* `JobName` - Name of job to get statistics for

Returns: `{ok, statistic()}` | `{error, not_found}`

Where statistic() contains:
- Job configuration
- Execution counts (ok/crashed/aborted/skipped)  
- Latest results
- Run times
- Next scheduled runs
```erlang
1> ecron:statistic(basic).
[#{name => basic,node => nonode@nohost,ok => 0,
   status => activate,type => cron,
   next => ["2025-02-20T22:15:00+08:00"...],        
   opts => [{singleton,false},{max_count,unlimited},{max_runtime_ms,unlimited}],
   mfa => {io,format,['Runs on 0, 15, 30, 45 minutes~n']},
   aborted => 0,crashed => 0,skipped => 0,
   start_time => {0,0,0},
   end_time => {23,59,59},
   run_microsecond => [],
   crontab =>
       #{second => [0],
         month => '*',
         minute => [0,15,30,45],
         hour => '*',day_of_month => '*',day_of_week => '*'},
   results => []}]
```
- `ok`: successful job.
- `crashed`: crashed job.
- `skipped`: singleton(true) job has been skipped due to the previous task still running.
- `aborted`: job has been aborted due to exceeding `max_runtime_ms`.

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
Retrieves statistics for both default and global registered jobs.

Returns: List of statistics for all jobs in both local and global registries.
Each statistic entry contains the same information as `statistic/2`.

```erlang
UniqueJobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.~n", []]},
{ok, UniqueJobName} = ecron:create(UniqueJobName, "0 4 * * *", MFA),
ecron:statistic(UniqueJobName).

[#{name => every_4am_job,node => nonode@nohost,ok => 0,
   status => activate,type => cron,
   next =>
       ["2025-02-19T04:00:00+08:00","2025-02-20T04:00:00+08:00",
        "2025-02-21T04:00:00+08:00","2025-02-22T04:00:00+08:00",
        ...
        ],
   opts => [{singleton,false}, {max_count,unlimited}, {max_runtime_ms,unlimited}],
   mfa => {io,format,["Run at 04:00 everyday.~n",[]]},
   aborted => 0,crashed => 0,skipped => 0, run_microsecond => [],
   start_time => {0,0,0},
   end_time => {23,59,59},
   crontab =>
       #{second => [0],
         month => '*',
         minute => [0],
         hour => [4],
         day_of_month => '*',day_of_week => '*'},
   results => []}]
```    
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
- Apply to both default and global registries

Returns: `ok`
""").
-spec reload() -> ok.
reload() ->
    gen_server:cast(?LocalJob, reload),
    gen_server:cast({global, ?GlobalJob}, reload).

?DOC("""
Reloads all tasks manually. Useful when system time has changed significantly.

Parameters:
* `Register` - Process name where job is registered

Returns: `ok`

""").
-spec reload(register()) -> ok.
reload(Register) ->
    gen_server:cast(Register, reload).

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
    {ok, #{type => cron | every, crontab => crontab(), next => [rfc3339_string()]}}
    | {error, atom(), term()}.
parse_spec(Spec, Num) when is_integer(Num) andalso Num > 0 ->
    parse_spec2(ecron_spec:parse_spec(Spec), Num).

parse_spec2({ok, Type, JobSpec}, Num) ->
    Job = #{type => Type, crontab => JobSpec},
    {ok, Next} = predict_datetime(Job, Num),
    {ok, Job#{next => Next}};
parse_spec2({error, _Field, _Value} = Error, _Num) ->
    Error.

get_next_schedule_time(Name) -> gen_server:call(?LocalJob, {next_schedule_time, Name}, infinity).

?DOC(false).
clear() -> gen_server:call(?LocalJob, clear, infinity).

?DOC(false).
predict_datetime(Job, Num) ->
    predict_datetime(Job, Num, {0, 0, 0}, {23, 59, 59}).

?DOC(false).
predict_datetime(Job, Num, Start, End) ->
    TZ = get_time_zone(),
    Now = current_millisecond(),
    predict_datetime(activate, Job, Start, End, Num, TZ, Now).

%%%===================================================================
%%% CallBack
%%%===================================================================
?DOC("""
Create a new registry.
Same as [`start_link(Register, [])`](#start_link/2).
Parameters:
* `Register` - Process name where job is registered

Returns: `{ok, Pid}` | `{error, Reason}`

""").
-spec start_link(register() | {local | global, register()}) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(Register) when is_atom(Register) ->
    start_link({local, Register}, []).

?DOC("""
Starts an new registry.

Parameters:
* `Name` - Process name where job is registered
* `JobSpec` - Crontab expression to parse list

Returns: `{ok, Pid}` | `{error, Reason}`

""").
-spec start_link(atom() | {local | global, atom()}, [{name(), crontab_spec(), mfargs()}]) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(JobTab, JobSpec) when is_atom(JobTab) ->
    start_link({local, JobTab}, JobSpec);
start_link({_, JobTab} = Name, JobSpec) ->
    case ecron_spec:parse_crontab(JobSpec, []) of
        {ok, Jobs} ->
            new_job_tab(JobTab, ets:info(JobTab)),
            gen_server:start_link(Name, ?MODULE, [JobTab, Jobs], []);
        {error, Reason} ->
            {error, Reason}
    end.

?DOC(false).
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
?DOC(false).
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
?DOC(false).
handle_info(timeout, State) ->
    {noreply, State, tick(State)};
handle_info({'EXIT', Pid, _Reason}, State = #state{timer_tab = TimerTab, job_tab = JobTab}) ->
    delete_pid(Pid, TimerTab, JobTab),
    {noreply, State, next_timeout(State)};
handle_info(_Unknown, State) ->
    {noreply, State, next_timeout(State)}.
?DOC(false).
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
                max_runtime_ms = proplists:get_value(max_runtime_ms, Opts),
                name = Name,
                mfa = MFA,
                link = PidOrUndef
            },
            Now = current_millisecond(),
            telemetry:execute(?Activate, #{action_at => Now}, #{name => Name, mfa => MFA}),
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
            telemetry:execute(?Deactivate, #{action_at => current_millisecond()}, #{name => Name}),
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
            telemetry:execute(?Delete, #{action_at => current_millisecond()}, #{name => Name}),
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
    {ok, NextSec} = next_schedule_millisecond(Type, Spec, TimeZone, Now, Start, End, Now),
    Timer = InitTimer#timer{
        key = {NextSec, Name},
        type = Type,
        spec = Spec,
        start_time = Start,
        end_time = End
    },
    ets:insert(TimeTab, Timer),
    {ok, Name}.

next_schedule_millisecond(every, Sec, TimeZone, Now, Start, End, InitMs) ->
    Next = Now + Sec * 1000,
    case Start =:= {0, 0, 0} andalso End =:= {23, 59, 59} of
        true ->
            {ok, Next};
        false ->
            {_, {NowHour, NowMin, NowSec}} = millisecond_to_datetime(TimeZone, Next),
            {StartHour, StartMin, StartSec} = Start,
            {EndHour, EndMin, EndSec} = End,
            NowTime = NowHour * 3600 + NowMin * 60 + NowSec,
            StartTime = StartHour * 3600 + StartMin * 60 + StartSec,
            EndTime = EndHour * 3600 + EndMin * 60 + EndSec,
            case NowTime >= StartTime andalso NowTime =< EndTime of
                true -> {ok, Next};
                false -> next_schedule_millisecond(every, Sec, TimeZone, Next, Start, End, InitMs)
            end
    end;
next_schedule_millisecond(cron, Spec, TimeZone, Now, Start, End, InitMs) ->
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
    next_schedule_millisecond2(Spec, MinSpec, ForwardDateTime, Start, End, TimeZone, InitMs).

next_schedule_millisecond2(Spec, MinSpec, ForwardDateTime, Start, End, TimeZone, InitMs) ->
    NextDateTime =
        {_, {NH, NM, NS}} =
        next_schedule_datetime(Spec, MinSpec, ForwardDateTime, Start, End),
    Next = NH * 3600 + NM * 60 + NS,
    {SHour, SMin, SSec} = Start,
    {EHour, EMin, ESec} = End,
    NextMs = datetime_to_millisecond(TimeZone, NextDateTime),
    case
        Next >= SHour * 3600 + SMin * 60 + SSec andalso
            Next =< EHour * 3600 + EMin * 60 + ESec
    of
        true ->
            {ok, NextMs};
        false when NextMs - InitMs =< 5 * 365 * 24 * 3600 * 1000 ->
            next_schedule_millisecond2(Spec, MinSpec, NextDateTime, Start, End, TimeZone, InitMs);
        false ->
            {error, "cant find next schedule time in the next 5 years"}
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
    #timer{
        singleton = Singleton,
        mfa = MFA,
        max_runtime_ms = MaxRuntimeMs,
        max_count = MaxCount,
        cur_count = CurCount
    } = Cron,
    ets:delete(TimerTab, Key),
    {Incr, CurPid} = maybe_spawn_worker(
        Cur - Due < 1000, Singleton, Name, MFA, MaxRuntimeMs, JobTab
    ),
    update_next_schedule(CurCount + Incr, MaxCount, Cron, Cur, Name, TZ, CurPid, TimerTab, JobTab),
    tick(State).

maybe_spawn_worker(true, _, Name, {erlang, send, Args}, MaxRuntimeMs, JobTab) ->
    {1, spawn_mfa(JobTab, Name, {erlang, send, Args}, MaxRuntimeMs)};
maybe_spawn_worker(true, true, Name, MFA, MaxRuntimeMs, JobTab) ->
    {1, spawn(?MODULE, spawn_mfa, [JobTab, Name, MFA, MaxRuntimeMs])};
maybe_spawn_worker(true, false, Name, MFA, MaxRuntimeMs, JobTab) ->
    spawn(?MODULE, spawn_mfa, [JobTab, Name, MFA, MaxRuntimeMs]),
    {1, false};
maybe_spawn_worker(true, Pid, Name, MFA, MaxRuntimeMs, JobTab) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            telemetry:execute(
                ?Skipped,
                #{
                    job_last_pid => Pid,
                    reason => "last task running, use {singleton, false} for concurrent runs",
                    action_at => current_millisecond()
                },
                #{
                    name => Name,
                    mfa => MFA
                }
            ),
            ets:update_counter(JobTab, Name, {#job.skipped, 1}),
            {0, Pid};
        false ->
            {1, spawn(?MODULE, spawn_mfa, [JobTab, Name, MFA, MaxRuntimeMs])}
    end;
maybe_spawn_worker(false, Singleton, _Name, _MFA, _MaxRuntimeMs, _JobTab) ->
    {0, Singleton}.

update_next_schedule(Max, Max, _Cron, _Cur, Name, _TZ, _CurPid, Tab, JobTab) ->
    delete_job(JobTab, Tab, Name);
update_next_schedule(Count, _Max, Cron, Cur, Name, TZ, CurPid, Tab, _JobTab) ->
    #timer{type = Type, start_time = Start, end_time = End, spec = Spec} = Cron,
    {ok, Next} = next_schedule_millisecond(Type, Spec, TZ, Cur, Start, End, Cur),
    NextTimer = Cron#timer{key = {Next, Name}, singleton = CurPid, cur_count = Count},
    ets:insert(Tab, NextTimer).

?DOC(false).
spawn_mfa(JobTab, Name, MFA, unlimited) ->
    spawn_mfa(JobTab, Name, MFA);
spawn_mfa(JobTab, Name, MFA, MaxRuntimeMs) ->
    {Pid, Ref} = spawn_monitor(?MODULE, spawn_mfa, [JobTab, Name, MFA]),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after MaxRuntimeMs ->
        case ets:lookup(JobTab, Name) of
            [] ->
                ok;
            [_] ->
                telemetry:execute(
                    ?Aborted,
                    #{
                        run_microsecond => MaxRuntimeMs,
                        action_at => current_millisecond()
                    },
                    #{
                        name => Name,
                        mfa => MFA
                    }
                ),
                ets:update_counter(JobTab, Name, {#job.aborted, 1})
        end,
        exit(Pid, kill)
    end.

?DOC(false).
spawn_mfa(JobTab, Name, MFA) ->
    Start = erlang:monotonic_time(),
    {Event, OkInc, CrashedInc, NewRes} =
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
                {?Crashed, 0, 1, {Error, Reason, Stacktrace}}
        end,
    End = erlang:monotonic_time(),
    Cost = erlang:convert_time_unit(End - Start, native, microsecond),
    telemetry:execute(
        Event,
        #{run_microsecond => Cost, action_at => current_millisecond(), run_result => NewRes},
        #{
            name => Name,
            mfa => MFA
        }
    ),
    case ets:lookup(JobTab, Name) of
        [] ->
            ok;
        [Job] ->
            #job{ok = Ok, crashed = Crashed, run_microsecond = RunMs, result = Results} = Job,
            Elements = [
                {#job.ok, Ok + OkInc},
                {#job.crashed, Crashed + CrashedInc},
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
    {ok, []};
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
    {ok, lists:reverse(Acc)};
predict_datetime2(Job, TimeZone, Now, Start, End, Num, Acc) ->
    #{type := Type, crontab := Spec} = Job,
    case next_schedule_millisecond(Type, Spec, TimeZone, Now, Start, End, Now) of
        {ok, Next} ->
            NewAcc = [to_rfc3339(Next) | Acc],
            predict_datetime2(Job, TimeZone, Next, Start, End, Num - 1, NewAcc);
        {error, Reason} ->
            {error, Reason}
    end.

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
        crashed = Crashed,
        skipped = Skipped,
        aborted = Aborted,
        result = Res,
        run_microsecond = RunMs
    } = Job,
    #{start_time := StartTime, end_time := EndTime} = JobSpec,
    {ok, Predict} = predict_datetime(Status, JobSpec, StartTime, EndTime, ?MAX_SIZE, TimeZone, Now),
    JobSpec#{
        status => Status,
        ok => Ok,
        crashed => Crashed,
        aborted => Aborted,
        skipped => Skipped,
        opts => Opts,
        next => Predict,
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
        {0, self()},
        maybe_spawn_worker(false, self(), test_name, {erlang, datetime, []}, 1000, job_tab)
    ).
-endif.
