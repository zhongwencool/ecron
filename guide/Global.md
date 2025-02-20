# Unique Job In Cluster

### Precondition

Global jobs depend on [global](http://erlang.org/doc/man/global.html), only allowed to be added statically.

1. Fully Connected Cluster
 
   Because it depends on `global`'s name registration service. 
   The name server must maintains a fully connected network. 

   For example, if node N1 connects to node N2 (which is already connected to N3), 
   the global name servers on the nodes N1 and N3 ensure that also N1 and N3 are connected. 
   In other words, command-line flag `-connect_all false` can not be used.

2. Same Global Configuration
 
   All node's `global_quorum_size` and `global_jobs` must keep the same value. 
   This ensures that the global task manager can transfer between nodes when the network splits.

### Configuration

#### global_jobs
<!-- tabs-open -->
### Erlang
```erlang
%% sys.config
[
   {ecron, [
      {global_jobs, [
         %% {JobName, CrontabSpec, {M, F, A}}
         %% {JobName, CrontabSpec, {M, F, A}, PropListOpts}
         %% CrontabSpec
            %%  1. "Minute Hour DayOfMonth Month DayOfWeek"
            %%  2. "Second Minute Hour DayOfMonth Month DayOfWeek"
            %%  3. @yearly | @annually | @monthly | @weekly | @daily | @midnight | @hourly
            %%  4. @every 1h2m3s
                  
         {basic, "*/15 * * * *", {io, format, ["Runs on 0, 15, 30, 45 minutes~n"]}}         
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
    
    {:basic, "*/15 * * * *", {IO, :puts, ["Runs on 0, 15, 30, 45 minutes"]}}   
  ] 
```
<!-- tabs-close -->

the same format as `local_jobs`, default is `[]`. 
This means only run local jobs without running global task manager and monitor processes.

#### global_quorum_size
ecron application live on at least `global_quorum_size` nodes in the same cluster, can be regarded as a healthy cluster. 
Global task manager only running on a healthy cluster.

If you want to guarantee always no more than **one** global task manager even when the cluster has network split,
you should set it to **"half plus one"**. For example:

Run on majority:
   1. `ABC` 3 nodes in one cluster.
   2. `global_quorum_size=2`.
   3. (`ABC`) cluster split into 2 part(`AB`  =|=  `C`).
   4. the global task manager would run on `AB` cluster(`AB` is the healthy cluster now).
   5. `C` node only running local jobs without global jobs.

Run on majority 
   1. `ABC` 3 nodes in one cluster.
   2. `global_quorum_size=2`.
   3. (`ABC`) cluster split into 3 part(`A` =|= `B`  =|=  `C`).
   4. every node only running local jobs without global jobs(all nodes is unhealthy).

Run on every node if brain split.
   1. `ABC` nodes in one cluster.    
   2. `global_quorum_size=1`.
   3. (`ABC`) cluster split into 3 part(`A` =|= `B`  =|=  `C`).
   4. the global task manager would run on every nodes(we have three healthy cluster now).
   5. But the global task manager only running one in the same cluster.

### Implementation
1. The top supervisor `ecron_sup` start at first.
2. Nothing will happen if the `global_jobs` is empty.
3. `ecron_sup` would start_link `ecron_monitor` worker (gen_server) if `global_jobs` is not empty.
4. `ecron_monitor` subscribes node's up/down messages by [net_kernel:monitor_nodes(true)](http://erlang.org/doc/man/net_kernel.html#monitor_nodes-1), when it initializes.
5. Enter `ecron_monitor` main loop:
    * Checking if there is enough `ecron` process in the cluster(`global_quorum_size`).
    * Trying to terminate global_job manager process when cluster's `ecron` number less than `global_quorum_size`.
    * Otherwise, trying to start a global_job manager process, This gen_server register by [global:register_name/2](http://erlang.org/doc/man/global.html#register_name-2).
    * All the nodes are rushing to register this global jobs manager process, only one node will success, other node's `ecron_monitor` would link this process if the process already exists.
    * The `ecron_monitor` should receive notification, when node down/up or the global_job manager has terminated.
    * Goto step 5 again, When notified.

```
     NodeA             NodeB             NodeC
   supervisor        supervisor        supervisor
       |                |     |            |
    monitor          monitor  |          monitor
         \              |     |            /
          \           [link]  |           /
           \            |     |          /
           [link]-----GlobalJob-----[link]
``` 
NodeB has won the race and spawn global manager process, other node's `ecron_monitor` only link to this manager.