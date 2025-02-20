# Implementation

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
    ecron_sup-------> ecron (registered as `ecron_local`)
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

