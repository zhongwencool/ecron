 ## Telemetry
 Ecron uses [Telemetry](https://github.com/beam-telemetry/telemetry) for instrumentation and for having an extensible way of doing logging. 
 Telemetry is a metrics and instrumentation library for Erlang and Elixir applications 
 that is based on publishing events through a common interface and attaching handlers to handle those events. 
 For more information about the library itself, see its [README](https://github.com/beam-telemetry/telemetry).
 
## Writing your own handler
If you want control on how ecron events are logged or on what level they're logged at, 
you can use your own event handler. For example, you can create a module to handle these events:

```erlang
-module(my_ecron_telemetry_logger).
-include_lib("kernel/include/logger.hrl").
-define(Events, [[ecron, success], [ecron, failure], [ecron, activate], 
                 [ecron, deactivate], [ecron, delete], 
                 [ecron, global, up], [ecron, global, down]]).
%% API
-export([attach/0, detach/0]).
-define(TELEMETRY_HANDLE, ecron_telemetry_metrics).
attach() ->
    telemetry:attach_many(?TELEMETRY_HANDLE, ?Events, fun handle_event/4, undefined).

detach() -> telemetry:detach(?TELEMETRY_HANDLE).

handle_event([ecron, activate], #{action_ms := Time}, #{name := Name, mfa := MFA}, _Config) ->
    ?LOG_INFO("EcronJob(~p)-~p activated at ~p .", [Name, MFA, Time]);
handle_event([ecron, deactivate], #{action_ms := Time}, #{name := Name}, _Config) ->
    ?LOG_INFO("EcronJob(~p) deactivated at ~p .", [Name, Time]);
handle_event([ecron, delete], #{action_ms := Time}, #{name := Name}, _Config) ->
    ?LOG_INFO("EcronJob(~p) deleted at ~p .", [Name, Time]);
handle_event([ecron, success], #{run_microsecond := Ms, run_result := Res},
    #{name := Name, mfa := {M, F, A}}, _Config) ->
    ?LOG_INFO("EcronJob(~p) completed in ~p microsecond.~p=apply(~p,~p,~p)", [Name, Ms, Res, M, F, A]);
handle_event([ecron, success], #{run_microsecond := Ms, run_result := Res},
    #{name := Name, mfa := {F, A}}, _Config) ->
    ?LOG_INFO("EcronJob(~p) completed in ~p microsecond.~p=apply(~p,~p)", [Name, Ms, Res, F, A]);
handle_event([ecron, failure], #{run_microsecond := Ms, run_result := {Error, Reason, Stack}},
    #{name := Name, mfa := MFA}, _Config) ->
    ?LOG_ERROR("EcronJob(~p)-~p CRASH in ~p microsecond. {Error, Reason}: {~p, ~p}. Stack:~p",
        [Name, MFA, Ms, Error, Reason, Stack]);
handle_event([ecron, global, up], #{action_ms := Time, quorum_size := QuorumSize,
    good_nodes := GoodNodes, bad_nodes := BadNodes}, #{self := Node}, _Config) ->
    ?LOG_INFO("Ecron Global UP on ~p at ~p quorum_size is ~p good_nodes is ~p bad_nodes is ~p ~n.",
        [Node, Time, QuorumSize, GoodNodes, BadNodes]);
handle_event([ecron, global, down], #{action_ms := Time, quorum_size := QuorumSize,
    good_nodes := GoodNodes, bad_nodes := BadNodes}, #{self := Node}, _Config) ->
    ?LOG_INFO("Ecron Global DOWN on ~p at ~p quorum_size is ~p good_nodes is ~p bad_nodes is ~p ~n.",
        [Node, Time, QuorumSize, GoodNodes, BadNodes]).
``` 

Once you have a module like this, you can attach it when your application starts:
```erlang
-module(my_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    my_ecron_telemetry_logger:attach(),
    my_sup:start_link().

stop(_State) ->
    my_ecron_telemetry_logger:detach(),
    ok.
```

More detail can be seen in [erlang example](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang/apps/titan/src/titan_ecron_logger.erl) and [elixir example](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir/apps/titan/lib/titan_ecron_logger.ex).

### Metrics
|     Event   | measurements                             | metadata                    | Describe                                            |
| ----------- | ---------------------------------------- | --------------------------- | --------------------------------------------------- |
| success     |#{run_microsecond=>Cost,run_result=>Res}  | #{name=>Name,mfa=>MFA}      | Execute MFA successfully                            |
| failure     |#{run_microsecond=>Cost,run_result=>Res}  | #{name=>Name,mfa=>MFA}      | MFA crashed(unsuccessfully)                         |
| activate    |#{action_ms=>Now}                         | #{name=>Name,mfa=>MFA}      | ecron:add or ecron:activate                         |
| deactivate  |#{action_ms=>Now}                         | #{name=>Name}               | ecron:deactivate                                    |
| delete      |#{action_ms=>Now}                         | #{name=>Name}               | ecron:delete or CurrentTime =:= job's EndDateTime   |
| global,up   |GlobalMeasurements                        | #{self=>node()}             | Global manager process is up                        |
| global,down |GlobalMeasurements                        | #{self=>node()}             | Global manager process is down                      |


- `Now = erlang:system_time(millisecond)`.
- `GlobalMeasurements = #{action_ms=>Now,quorum_size=>integer(),good_nodes=>[node()],bad_nodes=>[node()]}`.