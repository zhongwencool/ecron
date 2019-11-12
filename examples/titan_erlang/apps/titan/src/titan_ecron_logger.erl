-module(titan_ecron_logger).

-include_lib("kernel/include/logger.hrl").
-define(Success, [ecron, success]).
-define(Failure, [ecron, failure]).
-define(Activate, [ecron, activate]).
-define(Deactivate, [ecron, deactivate]).
-define(Delete, [ecron, delete]).
-define(GlobalUp, [ecron, global, up]).
-define(GlobalDown, [ecron, global, down]).

-define(Events, [?Success, ?Failure, ?Activate, ?Deactivate, ?Delete, ?GlobalUp, ?GlobalDown]).
%% API
-export([attach/0, detach/0]).
-define(TELEMETRY_HANDLE, ecron_telemetry_metrics).

attach() ->
    telemetry:attach_many(?TELEMETRY_HANDLE, ?Events, fun handle_event/4, undefined).

detach() ->
    telemetry:detach(?TELEMETRY_HANDLE).

handle_event(?Activate, #{action_ms := Time}, #{name := Name, mfa := MFA}, _Config) ->
    ?LOG_INFO("EcronJob(~p)-~p activated at ~p .", [Name, MFA, Time]);
handle_event(?Deactivate, #{action_ms := Time}, #{name := Name}, _Config) ->
    ?LOG_INFO("EcronJob(~p) deactivated at ~p .", [Name, Time]);
handle_event(?Delete, #{action_ms := Time}, #{name := Name}, _Config) ->
    ?LOG_INFO("EcronJob(~p) deleted at ~p .", [Name, Time]);
handle_event(?Success, #{run_microsecond := Ms, run_result := Res},
    #{name := Name, mfa := {M, F, A}}, _Config) ->
    ?LOG_INFO("EcronJob(~p) completed in ~p microsecond.~p=apply(~p,~p,~p)", [Name, Ms, Res, M, F, A]);
handle_event(?Success, #{run_microsecond := Ms, run_result := Res},
    #{name := Name, mfa := {F, A}}, _Config) ->
    ?LOG_INFO("EcronJob(~p) completed in ~p microsecond.~p=apply(~p,~p)", [Name, Ms, Res, F, A]);
handle_event(?Failure, #{run_microsecond := Ms, run_result := {Error, Reason, Stack}},
    #{name := Name, mfa := MFA}, _Config) ->
    ?LOG_ERROR("EcronJob(~p)-~p CRASH in ~p microsecond. {Error, Reason}: {~p, ~p}. Stack:~p",
        [Name, MFA, Ms, Error, Reason, Stack]);
handle_event(?GlobalUp, #{action_ms := Time, quorum_size := QuorumSize,
    good_nodes := GoodNodes, bad_nodes := BadNodes}, #{self := Node}, _Config) ->
    ?LOG_INFO("Ecron Global UP on ~p at ~p quorum_size is ~p good_nodes is ~p bad_nodes is ~p.",
        [Node, Time, QuorumSize, GoodNodes, BadNodes]);
handle_event(?GlobalDown, #{action_ms := Time, quorum_size := QuorumSize,
    good_nodes := GoodNodes, bad_nodes := BadNodes}, #{self := Node}, _Config) ->
    ?LOG_INFO("Ecron Global DOWN on ~p at ~p quorum_size is ~p good_nodes is ~p bad_nodes is ~p.",
        [Node, Time, QuorumSize, GoodNodes, BadNodes]).
