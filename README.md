<!-- markdownlint-disable MD013 -->
# ecron [![GitHub Actions][action-img]][action] [![codeCov-img]][codeCov] [![hex-img]][hex] [![tag]][tag] [![License]][License]
[action]: https://github.com/zhongwencool/ecron
[action-img]: https://github.com/zhongwencool/ecron/actions/workflows/ci.yml/badge.svg
[codeCov]: https://codecov.io/gh/zhongwencool/ecron
[codeCov-img]: https://codecov.io/gh/zhongwencool/ecron/branch/master/graph/badge.svg?token=FI9WAQ6UG5
[hex]: https://hex.pm/packages/ecron
[hex-img]: https://img.shields.io/hexpm/v/ecron.svg?style=flat
[tag]: https://img.shields.io/github/tag/zhongwencool/ecron.svg
[License]: https://img.shields.io/hexpm/l/ecron.svg

A lightweight and efficient cron-like job scheduling library for Erlang.

## Overview
Ecron is designed to manage scheduled jobs within a single gen_server process, similar to the standard library's [stdlib's timer](http://erlang.org/doc/man/timer.html). It uses an ordered_set ETS table to organize jobs by the next run time, ensuring efficient execution. Unlike traditional cron, Ecoron does not poll the system every second, which reduces message overhead and process usage.

more detail see [Implementation](#Implementation).


### Key Features
- Supports both cron-like and interval-based scheduling.
- Well-tested with [PropTest](https://github.com/proper-testing/proper) [![codecov](https://codecov.io/gh/zhongwencool/ecron/branch/master/graph/badge.svg?token=FI9WAQ6UG5)](https://codecov.io/gh/zhongwencool/ecron).
- Utilizes gen_server timeout mechanism for precise timing.
- Efficient process management, avoiding high memory usage.

You can find a collection of general practices in [Full Erlang Examples](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang) and [Full Elixir Examples](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir).

## Installation
  
**Erlang**
 ```erlang
  %% rebar.config
  {deps, [ecron]}
 ```

**Elixir**
 ```elixir
  # mix.exs
  def deps do
    [{:ecron, "~> 1.0.1"}]
  end
```

## Basic Usage 

Configure Ecoron in your sys.config file with timezone and job specifications:

```erlang
%% sys.config
[
   {ecron, [      
      {time_zone, local}, %% local or utc
      {local_jobs, [
         %% {JobName, CrontabSpec, {M, F, A}}
         %% {JobName, CrontabSpec, {M, F, A}, StartDateTime, EndDateTime}
         %% CrontabSpec
            %%  1. "Minute Hour DayOfMonth Month DayOfWeek"
            %%  2. "Second Minute Hour DayOfMonth Month DayOfWeek"
            %%  3. @yearly | @annually | @monthly | @weekly | @daily | @midnight | @hourly
            %%  4. @every 1h2m3s
                  
         {crontab_job, "*/15 * * * *", {io, format, ["Runs on 0, 15, 30, 45 minutes~n"]}},
         {extend_crontab_job, "0 0 1-6/2,18 * * *", {io, format, ["Runs on 1,3,6,18 o'clock:~n"]}},
         {alphabet_job, "@hourly", {io, format, ["Runs every(0-23) o'clock~n"]}},    
         {fixed_interval_job, "@every 30m", {io, format, ["Runs every 30 minutes"]}},
         %% Runs every 15 minutes between {8,20,0} and {23, 59, 59}.
         %% {limit_time_job, "*/15 * * * *", {io, format, ["Runs 0, 15, 30, 45 minutes after 8:20am~n"]}, {8,20,0}, unlimited}
         %% parallel job         
         {no_singleton_job, "@minutely", {timer, sleep, [61000]}, unlimited, unlimited, [{singleton, false}]}            
     ]},
     {global_jobs, []}, %% Global Spec has the same format as local_jobs.
     {global_quorum_size, 1} %%  Minimum number of nodes which run ecron. Global_jobs only run on majority cluster when it > ClusterNode/2.
    }
].
```

* Default `time_zone` is `local`, the current datetime is [calendar:local_time()](http://erlang.org/doc/man/calendar.html#local_time-0).
* The current datetime is [calendar:universal_time()](http://erlang.org/doc/man/calendar.html#universal_time-0) when `{time_zone, utc}`.
* The job will be auto remove at `EndDateTime`, the default value of `EndDateTime` is `unlimited`. 
* Default job is singleton, Each task cannot be executed concurrently. 
* If the system clock suddenly alter a lot(such as sleep your laptop for two hours or modify system time manually),
  it will skip the tasks which are supposed be running during the sudden lapse of time,
  then recalculate the next running time by the latest system time.
  You can also reload task manually by `ecron:reload().` when the system time is manually modified.
* Global jobs depend on [global](http://erlang.org/doc/man/global.html), only allowed to be added statically, [check this for more detail](https://github.com/zhongwencool/ecron/blob/master/doc/global.md).

## Supervisor Tree Usage
Ecron can be integrated into your application's supervision tree for better control over its lifecycle:

```erlang
%%config/sys.config
[{your_app, [{crontab_jobs, [
   {crontab_job, "*/15 * * * *", {stateless_cron, inspect, ["Runs on 0, 15, 30, 45 minutes"]}}
  ]}
].

%% src/your_app_sup.erl
-module(your_app_top_sup).
-behaviour(supervisor).
-export([init/1]).

init(_Args) ->
    Jobs = application:get_env(your_app, crontab_jobs, []),
    SupFlags = #{
        strategy => one_for_one,
        intensity => 100,
        period => 30
    },
    Name = 'uniqueName',
    CronSpec = #{
        id => Name,
        start => {ecron, start_link, [Name, Jobs]},
        restart => permanent,
        shutdown => 1000,
        type => worker
    },
    {ok, {SupFlags, [CronSpec]}}.

```
## Advanced Usage 

Ecron allows for advanced scheduling and manipulation of cron jobs:

```erlang
%% Same as: Spec = "0 * 0-5,18 * * 0-5", 
Spec = #{second => [0], 
       minute => '*',   
       hour => [{0,5}, 18], %% same as [0,1,2,3,4,5,18]
       month => '*',
       day_of_month => '*',
       day_of_week => [{0,5}]},
CronMFA = {io, format, ["Runs on 0-5,18 o'clock between Sunday and Firday.~n"]},
%% with name crontab  
{ok, _} = ecron:add(crontabUniqueName, Spec, CronMFA),
%% or
{ok, _} = ecron:add(crontabUniqueName, "0 * 0-5,18 * * 0-5", CronMFA),   
ok = ecron:delete(crontabuniqueName),
%% crontab with startTime and endTime
StartDateTime = {{2019,9,19},{0,0,0}},
EndDateTime = {{2020,9,19},{0,0,0}},
{ok, _} = ecron:add(crontabUniqueName, Spec, CronMFA, StartDateTime, EndDateTime),
%% crontab without name
{ok, JobName} = ecron:add(Spec, CronMFA, StartDateTime, EndDateTime), 
ok = ecron:delete(crontabuniqueName),
%% Runs every 120 second (fixed interval)
EveryMFA = {io, format, ["Runs every 120 second.~n"]},
{ok, _} = ecron:add(everyUniqueName, 120, EveryMFA),
```
## Debug Support

Ecron provides functions to assist with debugging:

````erlang
1> ecron:deactivate(CrontabName).
ok

2> ecron:activate(CrontabName).
ok

3> ecron:statistic(CrontabName).
{ok,
  #{crontab =>
      #{day_of_month => '*',
       day_of_week => [{1,5}],hour => [1,13],
       minute => [0],month => '*',second => [0]},
       start_time => unlimited,end_time => unlimited,
       failed => 0,mfa => {io,format,["ddd"]},
       name => test,status => activate,type => cron,
       ok => 1,results => [ok],run_microsecond => [12],       
       opts => [{singleton,true}], node => 'test@127.0.0.1',
       next =>
          ["2019-09-27T01:00:00+08:00","2019-09-27T13:00:00+08:00",
           "2019-09-30T01:00:00+08:00","2019-09-30T13:00:00+08:00",
           "2019-10-01T01:00:00+08:00","2019-10-01T13:00:00+08:00",
           "2019-10-02T01:00:00+08:00","2019-10-02T13:00:00+08:00",
           [...]|...]      
      }
}

4> ecron:parse_spec("0 0 1,13 * * 1-5", 5).
{ok,
#{crontab =>
      #{day_of_month => '*',
        day_of_week => [{1,5}],
        hour => [1,13],
        minute => [0],
        month => '*',
        second => [0]},
  next =>
      ["2019-09-16T13:00:00+08:00","2019-09-17T01:00:00+08:00",
       "2019-09-17T13:00:00+08:00","2019-09-18T01:00:00+08:00",
       "2019-09-18T13:00:00+08:00"],
  type => cron}
}
````
## Implementation

Ecron uses an efficient approach to manage job execution times and intervals:

1. The top supervisor `ecron_sup` starts firstly.
2. then `ecron_sup` starts a child `ecron`(gen_server worker).
3. `ecron` will look for configuration `{local_jobs, Jobs}` at initialization.
4. For each crontab job found, determine the next time in the future that each command must run.
5. Place those commands on the ordered_set ets with their `{NextCorrespondingTime, Name}` to run as key.
6. Enter `ecron`'s main loop:
    * Examine the task entry at the head of the ets, compute how far in the future it must run.
    * Sleep for that period of time by gen_server timeout feature.
    * On awakening and after verifying the correct time, execute the task at the head of the ets (spawn in background).
    * Delete old key in ets.
    * Determine the next time in the future to run this command and place it back on the ets at that time value.
    
Additionally, you can use `ecron:statistic(Name)` to see the job's latest 16 results and execute times.
```
    ecron_sup------->ecron
                      |
                   |------| ets:new(timer, [ordered_set])
                   | init | ets:insert(timer, [{{NextTriggeredTime,Name}...])
                   |------|
             |------->|
             |     |------| {NextTriggeredTime,Name} = OldKey = ets:first(timer)
             |     |      | sleep(NextTriggeredTime - current_time())
             |     | loop | spawn a process to execute MFA
             |     |      | ets:delete(timer, OldKey)
             |     |------| ets:insert(timer, {{NewTriggeredTime,Name}...])
             |<-------|
```

[Check this for global_jobs workflow](https://github.com/zhongwencool/ecron/blob/master/doc/global.md#Implementation).       

## Telemetry

Ecron publishes events through telemetry, allowing for monitoring and alerting,
You can handle those events by [this guide](https://github.com/zhongwencool/ecron/blob/master/doc/telemetry.md).

## CRON Expression Format

A [cron expression](https://www.wikiwand.com/en/Cron) represents a set of times, using 5-6 space-separated fields.
Currently, W (nearest weekday), L (last day of month/week), and # (nth weekday of the month) are not supported. 

Most other features supported by popular cron implementations should work just fine.
```shell script
 # ┌────────────── second (optional)
 # │ ┌──────────── minute
 # │ │ ┌────────── hour
 # │ │ │ ┌──────── day of month
 # │ │ │ │ ┌────── month
 # │ │ │ │ │ ┌──── day of week
 # │ │ │ │ │ │
 # │ │ │ │ │ │
 # 0 * * * * *
```

Field name   | Mandatory? | Allowed values  | Allowed special characters
----------   | ---------- | --------------  | --------------------------
Seconds      | No         | 0-59            | * / , -
Minutes      | Yes        | 0-59            | * / , -
Hours        | Yes        | 0-23            | * / , -
Day of month | Yes        | 1-31            | * / , -
Month        | Yes        | 1-12 or JAN-DEC | * / , -
Day of week  | Yes        | 0-6 or SUN-SAT  | * / , -

Note: Month and Day-of-week field values are case-insensitive. "SUN", "Sun", and "sun" are equally accepted.

When specifying your cron values you'll need to make sure that your values fall within the ranges. 
For instance, some cron's use a 0-7 range for the day of week where both 0 and 7 represent Sunday. We do not.

### Special Characters
#### Asterisk ( * )
The asterisk indicates that the cron expression will match for all values of the field. 
For example, using an asterisk in the `month` field would indicate every month.

#### Slash ( / )
Slashes are used to describe increments of ranges. 
For example, "3-59/15" in the `minutes` field would indicate the 3rd minute of the hour and every 15 minutes thereafter. 
The form "*/..." is equivalent to the form "First-Last/...", that is, an increment over the largest possible range of the field. 
The form "N/..." is accepted as meaning "N-Max/...", that is, starting at N, use the increment until the end of that specific range. 
It does not wrap around.

#### Comma ( , )
Commas are used to separate items of a list. 
For example, using "MON,WED,FRI" in the `day_of_week` field would mean Mondays, Wednesdays and Fridays.

#### Hyphen ( - )
Hyphens are used to define ranges. 
For example, using "9-17" in the `hours`field  would indicate every hour between 9am and 5pm inclusive.

## Predefined crontab

You may use one of several pre-defined crontab in place of a cron expression.

Entry                  | Description                                | Equivalent To
-----                  | -----------                                | -------------
@yearly (or @annually) | Run once a year, midnight, Jan. 1st        | 0 0 0 1 1 *
@monthly               | Run once a month, midnight, first of month | 0 0 0 1 * *
@weekly                | Run once a week, midnight between Sat/Sun  | 0 0 0 * * 0
@daily (or @midnight)  | Run once a day, midnight                   | 0 0 0 * * *
@hourly                | Run once an hour, beginning of hour        | 0 0 * * * *
@minutely              | Run once an minute, beginning of minute    | 0 * * * * *

>There are tools that help when constructing your cronjobs. 
>You might find something like [https://crontab.guru/](https://crontab.guru/) or [https://cronjob.xyz/](https://cronjob.xyz/) helpful. 
>But, note that these don't necessarily accept the exact same syntax as this library, 
>for instance, it doesn't accept the seconds field, so keep that in mind.
>The best way to verify the spec format is `ecron:parse_spec("0 0 1 1 1-6 1", 10).`. 

## Intervals

Ecron also supports scheduling jobs at fixed intervals:

```shell
@every <duration>
```
For example, @every 1h30m10s schedules a job to run every 1 hour, 30 minutes, and 10 seconds.

>Note: The interval doesn't take the job runtime into account. 
>For example, if a job takes 3 minutes to run, and it is scheduled to run every 5 minutes, 
>it only has 2 minutes of idle time between each run.
  
## Test

This command will run property-based tests, common tests, and generate a coverage report with verbose output.

```shell
  $ rebar3 do proper -c, ct -c, cover -v
```
