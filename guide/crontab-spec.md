# Crontab Spec

## Basic Format

A cron expression consists of 5-6 fields representing time units:

```
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

### Field Values

Field | Required | Values | Special Characters
Second | No | 0-59 | `* / , -`
Minute | Yes | 0-59 | `* / , -`
Hour | Yes | 0-23 | `* / , -`
Day of Month | Yes | 1-31 | `* / , -`
Month | Yes | 1-12 or JAN-DEC | `* / , -`
Day of Week | Yes | 0-6 or SUN-SAT | `* / , -`

> **Note**: Month and Day-of-week values are case-insensitive.

### Special Characters

- `*` - Any value
- `/` - Step values (e.g., `*/15` - every 15 units)
- `,` - Value list (e.g., `1,3,5`)
- `-` - Range (e.g., `1-5`)

## Predefined Schedules

Expression | Description | Equivalent
`@yearly` | Once a year (midnight, Jan 1) | `0 0 0 1 1 *`
`@monthly` | Once a month (midnight, first day) | `0 0 0 1 * *`
`@weekly` | Once a week (midnight, Sunday) | `0 0 0 * * 0`
`@daily` | Once a day (midnight) | `0 0 0 * * *`
`@hourly` | Once an hour | `0 0 * * * *`
`@minutely` | Once a minute | `0 * * * * *`

## Fixed Intervals

For simpler scheduling needs, use `@every` with a duration:

```erlang
% Run every 30 minutes
"@every 30m"

% Run every 1 hour and 30 minutes
"@every 1h30m"
```

Duration units: `d` (days), `h` (hours), `m` (minutes), `s` (seconds)