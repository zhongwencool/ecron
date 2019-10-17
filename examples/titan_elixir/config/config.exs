# This file is responsible for configuring your umbrella
# and **all applications** and their dependencies with the
# help of the Config module.
#
# Note that all applications in your umbrella share the
# same configuration and dependencies, which is why they
# all use the same configuration file. If you want different
# configurations or dependencies per app, it is best to
# move said applications out of the umbrella.
import Config

# Sample configuration:
#
#     config :logger, :console,
#       level: :info,
#       format: "$date $time [$level] $metadata$message\n",
#       metadata: [:user_id]
#

# config :ecron, :time_zone, :utc
config :ecron, :time_zone, :local # or :utc

# {JobName, CrontabSpec, {M, F, A}}
# {JobName, CrontabSpec, {M, F, A}, StartDateTime, EndDateTime}
# CrontabSpec
#  1. "Minute Hour DayOfMonth Month DayOfWeek"
#  2. "Second Minute Hour DayOfMonth Month DayOfWeek"
#  3. @yearly | @annually | @monthly | @weekly | @daily | @midnight | @hourly | @minutely
#  4. @every 1h2m3s

config :ecron, :jobs,
       [
         # Standard crontab spec without second (default second is 0 not *).
         {:crontab_job, "*/15 * * * *", {StatelessCron, :inspect, ["Runs on 0, 15, 30, 45 minutes"]}},
         # Extend crontab spec with second.
         {:extend_crontab_job, "0 0 1-6/2,18 * * *", {StatelessCron, :inspect, ["Runs on 1,3,6,18 o'clock"]}},
         # Crontab spec with alphabet.
         {:alphabet_job, "@hourly", {StatelessCron, :inspect, ["Runs every(0-23) o'clock"]}},
         # Fixed interval spec.
         {:fixed_interval_job, "@every 5m", {StatelessCron, :inspect, ["Runs every 5 minutes"]}},
         # Job with startDateTime and EndDateTime. Runs 0-23 o'clock since {{2019,9,26},{0,0,0}}.
         {
           :limit_datetime_job,
           "@hourly",
           {StatelessCron, :inspect, ["Runs every(0-23) o'clock"]},
           {{2019, 9, 26}, {0, 0, 0}},
           :unlimited
         },
         # Job with max run count, default is `unlimited`
         {
           :max_run_count_job,
           "@daily",
           {StatelessCron, :inspect, ["Runs daily"]},
           :unlimited,
           :unlimited,
           max_count: 1000
         },
         # Parallel job, singleton default is true.
         {:no_singleton_job, "@minutely", {Process, :sleep, [61000]}, :unlimited, :unlimited, singleton: false}
       ]
