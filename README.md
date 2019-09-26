ecron
=====
[![Build Status](https://travis-ci.org/zhongwencool/ecron.png?branch=master)](https://travis-ci.org/zhongwencool/ecron)
[![Coverage Status](https://coveralls.io/repos/github/zhongwencool/ecron/badge.svg?branch=master)](https://coveralls.io/github/zhongwencool/ecron?branch=master)
[![Hex.pm](https://img.shields.io/hexpm/v/ecron.svg?style=flat)](https://hex.pm/packages/ecron)

A lightweight/efficient cron-like job scheduling library for Erlang.

Ecron does not poll the system on a minute-by-minute basis like cron does. 
All jobs is assigned to a single process, just run as same as the [timer](http://erlang.org/doc/man/timer.html).

It organize the tasks to be run in a ordered_set ets with the next time to run as key. 
This way, you only need one process that calculates the time to wait until the next task should be executed, 
then spawn the process to execute that task. Saves lots of processes.
 
This implementation also prevents a lot of messages from flying around.

It offers:

* Both cron-like scheduling and interval-based scheduling.
* Well tested by `PropTest` ![Coverage Status](https://coveralls.io/repos/github/zhongwencool/ecron/badge.svg?branch=master).
* Use gen_server timeout(`receive after`) at any given time (rather than reevaluating upcoming jobs every second/minute).
* Minimal overhead. ecron aims to keep its code base small.

Basic Usage 
-----
```erlang
%% sys.config
[
   {ecron, [      
      {time_zone, local}, %% local or utc
      {jobs, [                
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
         %% Runs 0-23 o'clock since {{2019,9,26},{0,0,0}}.
         {limit_datetime_job, "@hourly", {io, format, ["Runs every(0-23) o'clock~n"]}, {{2019,9,26},{0,0,0}}, unlimited}            
     ]}
]
```

* When `time_zone` is `local`, current datetime is [calendar:local_time()](http://erlang.org/doc/man/calendar.html#local_time-0).
* When `time_zone` is `utc`, current datetime is [calendar:universal_time()](http://erlang.org/doc/man/calendar.html#universal_time-0).
* The job will be auto remove at the end of the time. 
* It handles not very radical when the system clock is altered, all workers only adjust system time by `{adjusting_time_second, 604800}`.  
  Another way to take effect immediately on all jobs is by running `ecron:activate(Name)` manually. 

Advanced Usage 
-----
```erlang
Spec = #{second => [0], 
       minute => '*',   
       hour => [{0,5}, 18], %% same as [0,1,2,3,4,5,18]
       month => '*',
       day_of_month => '*',
       day_of_week => [{0,5}]},
CronMFA = {io, format, ["Runs on 0-5,18 o'clock between Sunday and Firday.~n"]},
%% crontab  
{ok, _} = ecron:add(crontabUniqueName, Spec, CronMFA), 
ok = ecron:delete(crontabuniqueName),
%% crontab with startTime and endTime
StartDateTime = {{2019,9,19},{0,0,0}},
EndDateTime = {{2020,9,19},{0,0,0}},
{ok, _} = ecron:add(crontabUniqueName, Spec, CronMFA, StartDateTime, EndDateTime), 
ok = ecron:delete(crontabuniqueName),
%% Runs every 120 second (fixed interval)
EveryMFA = {io, format, ["Runs every 120 second.~n"]},
{ok, _} = ecron:add(everyUniqueName, 120, EveryMFA),
```
Debug Support
------
There are some function to get information for a Job and to handle the Job and Invocations.
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
       ok => 0,results => [],run_microsecond => [],       
       opts => [{singleton,true}],
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

CRON Expression Format
-----
A cron expression represents a set of times, using 5-6 space-separated fields.
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
 # * * * * * *
```

Field name   | Mandatory? | Allowed values  | Allowed special characters
----------   | ---------- | --------------  | --------------------------
Seconds      | No         | 0-59            | * / , -
Minutes      | Yes        | 0-59            | * / , -
Hours        | Yes        | 0-23            | * / , -
Day of month | Yes        | 1-31            | * / , -
Month        | Yes        | 1-12 or JAN-DEC | * / , -
Day of week  | Yes        | 0-6 or SUN-SAT  | * / , -

Note: Month and Day-of-week field values are case insensitive. "SUN", "Sun", and "sun" are equally accepted.

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

Predefined crontab
------
You may use one of several pre-defined crontab in place of a cron expression.

Entry                  | Description                                | Equivalent To
-----                  | -----------                                | -------------
@yearly (or @annually) | Run once a year, midnight, Jan. 1st        | 0 0 0 1 1 *
@monthly               | Run once a month, midnight, first of month | 0 0 0 1 * *
@weekly                | Run once a week, midnight between Sat/Sun  | 0 0 0 * * 0
@daily (or @midnight)  | Run once a day, midnight                   | 0 0 0 * * *
@hourly                | Run once an hour, beginning of hour        | 0 0 * * * *

>There are tools that help when constructing your cronjobs. 
>You might find something like [https://crontab.guru/](https://crontab.guru/) or [https://cronjob.xyz/](https://cronjob.xyz/) helpful. 
>But, note that these don't necessarily accept the exact same syntax as this library, 
>for instance, it doesn't accept the seconds field, so keep that in mind.

Intervals
-----
You may also schedule a job to execute at fixed intervals, starting at the time it's added or cron is run. 
This is supported by formatting the cron spec like this:
```shell
@every <duration>
```
For example, "@every 1h30m10s" would indicate a schedule that activates after 1 hour, 30 minutes, 10 seconds, and then every interval after that.

>Note: The interval doesn't take the job runtime into account. 
>For example, if a job takes 3 minutes to run, and it is scheduled to run every 5 minutes, 
>it will have 5 minutes of idle time between each run.
  
Implementation
-----
TODO  explain `singleton` options.

Proper Test
-----

```shell
  $ rebar3 do proper -c, cover -v
```

TODO
----
* support the last day of a month.
* support `global` start
