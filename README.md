# Ecron [![GitHub Actions](https://github.com/zhongwencool/ecron/actions/workflows/ci.yml/badge.svg)](https://github.com/zhongwencool/ecron) [![CodeCov](https://codecov.io/gh/zhongwencool/ecron/branch/master/graph/badge.svg?token=FI9WAQ6UG5)](https://codecov.io/gh/zhongwencool/ecron) [![Hex](https://img.shields.io/hexpm/v/ecron.svg?style=flat)](https://hex.pm/packages/ecron) [![Tag](https://img.shields.io/github/tag/zhongwencool/ecron.svg)](https://img.shields.io/github/tag/zhongwencool/ecron.svg) [![License](https://img.shields.io/hexpm/l/ecron.svg)](https://img.shields.io/hexpm/l/ecron.svg) [![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/ecron/)

> Resilient, lightweight, efficient, cron-like job scheduling library for Erlang/Elixir.

![EcronLogo](https://github.com/user-attachments/assets/ad3b493b-c1ef-4528-b7be-1e074c17b993)

Ecron supports both cron-style and interval-based job scheduling, focus on resiliency, correctness and lightweight with comprehensive testing via PropTest.

## Use Case
Ecron's precise scheduling is perfect for:

- **Security**: `0 3 * * 0` Rotate API keys and dynamic credentials automatically every Sunday at 3 AM
- **Flash Sale**: `0 8 * * *` Launch flash sales with precision at 8 AM
- **Analytics**: `0 9 * * 1` Sending with comprehensive weekly reports At every Monday at 9 AM
- **Disk Protection**: `30 23 * * *` Compress and archive old logs at 23:30 daily
- **Data Cleanup**: `0 1 1 * *` Pruning inactive users on the first day of each month
- **Data Backup**: `0 2 * * *` Create reliable Mnesia database backups every day at 2 AM

## Setup

<!-- tabs-open -->
### Erlang

```erlang
  %% rebar.config
  {deps, [{ecron, "~> 1.1.0"}]}
```
### Elixir

```elixir
  # mix.exs
  def deps do
    [{:ecron, "~> 1.1.0"}]
  end
```
<!-- tabs-close -->

## Configuration Usage 

<!-- tabs-open -->
### Erlang

Configure ecron in your `sys.config` file with job specifications:

```erlang
%% sys.config
[
   {ecron, [
      {local_jobs, [
         %% {JobName, CrontabSpec, {M, F, A}}
         %% {JobName, CrontabSpec, {M, F, A}, PropListOpts}
         %% CrontabSpec
            %%  1. "Minute Hour DayOfMonth Month DayOfWeek"
            %%  2. "Second Minute Hour DayOfMonth Month DayOfWeek"
            %%  3. @yearly | @annually | @monthly | @weekly | @daily | @midnight | @hourly
            %%  4. @every 1h2m3s
                  
         {basic, "*/15 * * * *", {io, format, ["Runs on 0, 15, 30, 45 minutes~n"]}},
         {sec_in_spec, "0 0 1-6/2,18 * * *", {io, format, ["Runs on 1,3,6,18 o'clock:~n"]}},
         {hourly, "@hourly", {io, format, ["Runs every(0-23) o'clock~n"]}},    
         {interval, "@every 30m", {io, format, ["Runs every 30 minutes"]}},         
         {limit_time, "*/15 * * * *", {io, format, ["Runs 0, 15, 30, 45 minutes after 8:20am~n"]}, [{start_time, {8,20,0}}, {end_time, {23, 59, 59}}]},
         {limit_count, "@every 1m", {io, format, ["Runs 10 times"]}, [{max_count, 10}]},         
         {limit_concurrency, "@minutely", {timer, sleep, [61000]}, [{singleton, true}]},         
         {limit_runtime_ms, "@every 1m", {timer, sleep, [2000]}, [{max_runtime_ms, 1000}]}
     ]}     
    }
].
```
### Elixir
Configure ecron in your `config.exs` file with job specifications:
```elixir
# config/config.exs
config :ecron,  
  local_jobs: [
    # {job_name, crontab_spec, {module, function, args}}
    # {job_name, crontab_spec, {module, function, args}, PropListOpts}
    # CrontabSpec formats:
    #  1. "Minute Hour DayOfMonth Month DayOfWeek"
    #  2. "Second Minute Hour DayOfMonth Month DayOfWeek"
    #  3. @yearly | @annually | @monthly | @weekly | @daily | @midnight | @hourly
    #  4. @every 1h2m3s
    
    {:basic, "*/15 * * * *", {IO, :puts, ["Runs on 0, 15, 30, 45 minutes"]}},
    {:sec_in_spec, "0 0 1-6/2,18 * * *", {IO, :puts, ["Runs on 1,3,6,18 o'clock:"]}},
    {:hourly, "@hourly", {IO, :puts, ["Runs every(0-23) o'clock"]}},
    {:interval, "@every 30m", {IO, :puts, ["Runs every 30 minutes"]}},    
    {:limit_time, "*/15 * * * *", {IO, :puts, ["Runs 0, 15, 30, 45 minutes after 8:20am"]}, [{start_time, {8,20,0}}, {end_time, {23, 59, 59}}]},
    {:limit_count, "@every 1m", {IO, :puts, ["Runs 10 times"]}, [max_count: 10]},
    {:limit_concurrency, "@minutely", {Process, :sleep, [61000]}, [singleton: true]},
    {:limit_runtime_ms, "@every 1m", {Process, :sleep, [2000]}, [max_runtime_ms: 1000]}
  ] 
```
<!-- tabs-close -->

* When a job reaches its `max_count` limit, it will be automatically removed. By default, `max_count` is set to `unlimited`.
* By default, `singleton` is set to `false`, which means multiple instances of the same job can run concurrently. Set `singleton` to `true` to ensure only one instance runs at a time.

For all PropListOpts, refer to the documentation for [`ecron:create/4`](https://hexdocs.pm/ecron/ecron.html#create/4).

## Runtime Usage
Besides loading jobs from config files at startup, you can add jobs from your code.
<!-- tabs-open -->
### Erlang
```erlang
JobName = every_4am_job,
MFA = {io, format, ["Run at 04:00 everyday.\n"]},
Options = #{max_runtime_ms => 1000},
ecron:create(JobName, "0 4 * * *", MFA, Options).
Statistic = ecron:statistic(JobName),
ecron:delete(JobName),
Statistic.
```
### Elixir
```elixir
job_name = :every_4am_job
mfa = {IO, :puts, ["Run at 04:00 everyday.\n"]}
options = %{max_runtime_ms: 1000}
{:ok, ^job_name} = :ecron.create(job_name, "0 4 * * *", mfa, options)
statistic = :ecron.statistic(job_name)
:ecron.delete(job_name)
statistic
```
<!-- tabs-close -->

### Multi Register
For most applications, the above two methods are enough. However, Ecron offers a more flexible way to manage job lifecycles.

For example, when applications A and B need separate cron jobs, you can create a dedicated register for each. This ensures jobs are removed when their parent application stops.

<!-- tabs-open -->
### Erlang
```erlang
{ok, _}= ecron:start_link(YourRegister),
ecron:create(YourRegister, JobName, Spec, MFA, Options),
ecron:delete(YourRegister, JobName).
```
### Elixir
```elixir
{:ok, _} = :ecron.start_link(YourRegister)
:ecron.create(YourRegister, JobName, Spec, MFA, Options)
:ecron.delete(YourRegister, JobName)
```
<!-- tabs-close -->

Alternatively, use a supervisor:

<!-- tabs-open -->
### Erlang
[supervisor:child_spec/0](https://www.erlang.org/doc/apps/stdlib/supervisor.html#t:child_spec/0)
```erlang
YourRegister = your_ecron_register,
Children = [
  #{
        id => YourRegister,
        start => {ecron, start_link, [YourRegister]},
        restart => permanent,
        shutdown => 1000,
        type => worker
    }
]
```
### Elixir
[supervisor:child_spec/1](https://hexdocs.pm/elixir/1.15.8/Supervisor.html#module-child_spec-1-function)
```elixir
children = [
  worker(:ecron, [:your_ecron_register], restart: :permanent)
]

```
<!-- tabs-close -->

After setup, use [`ecron:create/4`](https://hexdocs.pm/ecron/ecron.html#create/4) and [`ecron:delete/2`](https://hexdocs.pm/ecron/ecron.html#delete/2) to manage your jobs.

## Time Zone
<!-- tabs-open -->
### Erlang

Configure ecron in your `sys.config` file with timezone and job specifications:

```erlang
%% sys.config
[
   {ecron, [      
      {time_zone, local} %% local or utc
   ]}
].
```
### Elixir
```elixir
# config/config.exs
config :ecron,
  time_zone: :local  # :local or :utc

```
<!-- tabs-close -->
* When `time_zone` is set to `local` (default), ecron uses [calendar:local_time()](http://erlang.org/doc/man/calendar.html#local_time-0) to get the current datetime in your system's timezone
* When `time_zone` is set to `utc`, ecron uses [calendar:universal_time()](http://erlang.org/doc/man/calendar.html#universal_time-0) to get the current datetime in UTC timezone

## Troubleshooting

Ecron provides functions to assist with debugging at running time:
[`ecron:statistic/x ecron:parse_spec/2`](https://hexdocs.pm/ecron/ecron.html#debugging-testing)

## Telemetry
Ecron uses [Telemetry](https://github.com/beam-telemetry/telemetry) for instrumentation and logging.
Telemetry is a metrics and instrumentation library for Erlang and Elixir applications 
that is based on publishing events through a common interface and attaching handlers to handle those events. 
For more information about the library itself, see its [README](https://github.com/beam-telemetry/telemetry).

Ecron logs all events by default. 
### Events
|  Event      | measurements map key                       | metadata map key | log level
| success     |run_microsecond,run_result,action_at        | name,mfa         | notice
| activate    |action_at                                   | name,mfa         | notice
| deactivate  |action_at                                   | name             | notice
| delete      |action_at                                   | name             | notice
| crashed     |run_microsecond,run_result,action_at        | name,mfa         | error
| skipped     |job_last_pid,reason,action_at               | name,mfa         | error
| aborted     |run_microsecond,action_at                   | name,mfa         | error
| global,up   |quorum_size,good_nodes,bad_nodes,action_at  | self(node)       | alert
| global,down |quorum_size,good_nodes,bad_nodes,action_at  | self(node)       | alert

For all failed event, refer to the documentation for [`ecron:statistic/2`](https://hexdocs.pm/ecron/ecron.html#statistic/2).

You can enable or disable logging via `log` configuration.
<!-- tabs-open -->
### Erlang
```erlang
  %% sys.config
  [
   {ecron, [
    %% none | all | alert | error | notice 
      {log_level, all}
   ]}
].
```
### Elixir
```elixir
# config/config.exs
config :ecron,
  # :none | :all | :alert | :error
  log: :all
```
<!-- tabs-close -->
- **all**: Logs all events.
- **none**: Logs no events.
- **alert**: Logs global(up/down) events.
- **error**: Logs crashed, skipped, aborted, and global(up/down) events.

> ### How ? {: .info}
> Use [logger:set_module_level(ecron_telemetry_logger, Log)](https://www.erlang.org/doc/apps/kernel/logger.html#set_module_level/2) to overrides the primary log level of Logger for log events originating from the ecron_telemetry_logger module.
 
### Writing your own handler
If you want custom logging control, you can create your own event handler. 
See [`src/ecron_telemetry_logger.erl`](https://github.com/zhongwencool/ecron/blob/main/src/ecron_telemetry_logger.erl) as a reference implementation.


## Contributing
To run run property-based tests, common tests, and generate a coverage report with verbose output.
```shell
  $ rebar3 do proper -c, ct -c, cover -v
```
It's take about 10-15 minutes.

## License
Ecron is released under the Apache-2.0 license. See the [license file](LICENSE).



