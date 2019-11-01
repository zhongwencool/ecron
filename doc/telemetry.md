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
-define(Events, [[ecron, success], [ecron, failure], [ecron, activate], [ecron, deactivate], [ecron, delete]]).
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
        [Name, MFA, Ms, Error, Reason, Stack]).
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

More detail can be seen in [erlang example](https://github.com/zhongwencool/ecron/blob/master/examples/titan_erlang/apps/titan/src/titan_ecron_logger.erl) and [elixir example](https://github.com/zhongwencool/ecron/blob/master/examples/titan_elixir/apps/titan/lib/titan/titan_cron_logger.ex). 


 
