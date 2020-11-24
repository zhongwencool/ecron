-module(prop_ecron_app_config).
-include_lib("proper/include/proper.hrl").
%% API
-export([prop_application_ok_config/0, prop_application_ok_config/1]).
-export([prop_application_error_config/0, prop_application_error_config/1]).

prop_application_ok_config(doc) -> "ecron application load normal config failed";
prop_application_ok_config(opts) -> [{numtests, 100}].
prop_application_ok_config() ->
    ?FORALL({Spec, UseTime}, {prop_ecron_spec:standard_spec(), bool()},
        ?IMPLIES(prop_ecron:check_day_of_month(Spec),
            begin
                error_logger:tty(false),
                application:start(telemetry),
                application:stop(ecron),
                SpecStr = prop_ecron:spec_to_str(Spec),
                Jobs =
                    case UseTime of
                        true -> [{noraml, SpecStr, {io_lib, format, ["~s", [SpecStr]]}, unlimited, unlimited}];
                        false -> [{normal, SpecStr, {io_lib, format, ["~s", SpecStr]}}]
                    end,
                TimeZone = application:get_env(ecron, time_zone, local),
                application:set_env(ecron, time_zone, utc),
                application:set_env(ecron, local_jobs, Jobs),
                Result = application:start(ecron),
                
                application:set_env(ecron, time_zone, TimeZone),
                application:set_env(ecron, local_jobs, []),
                
                error_logger:tty(true),
                ?WHENFAIL(
                    io:format("Start ~s Failed: ~p\n", [SpecStr, Result]),
                    Result =:= ok
                )
            end)
    ).

prop_application_error_config(doc) -> "ecron application load error config failed";
prop_application_error_config(opts) -> [{numtests, 800}].
prop_application_error_config() ->
    ?FORALL({Spec, UseTime}, {prop_ecron_spec:maybe_error_spec(), range(0, 3)},
        begin
            error_logger:tty(false),
            application:start(telemetry),
            application:stop(ecron),
            SpecStr = prop_ecron:spec_to_str(Spec),
            Jobs =
                case UseTime of
                    0 -> [{test, error_format}];
                    1 -> [{test, SpecStr, {io_lib, format, ["~s", SpecStr]}}];
                    2 -> [{test, "* * * * * *", {io_lib, format, ["~s", SpecStr]}, {2019, 12, 10}, unlimited}];
                    3 -> [{test, test, {io_lib, format, ["~s", SpecStr]}}]
                end,
            application:set_env(ecron, local_jobs, Jobs),
            T = application:start(ecron),
            Res =
                case T of
                    {error, {{shutdown,
                        {failed_to_start_child, ecron,
                            Reason}}, {ecron_app, start, [normal, []]}}} ->
                        lists:member(Reason, [
                            {test, error_format},
                            "invalid_time: {{2019,12,10},unlimited}",
                            "invalid_spec: test"]) orelse check_spec(Reason, SpecStr);
                    ok ->
                        true
                end,
            error_logger:tty(true),
            Res
        end).

check_spec(Actual, SpecStr) ->
    {error, Field, Reason} = ecron_spec:parse_spec(SpecStr),
    Expect = lists:flatten(io_lib:format("~p: ~p", [Field, Reason])),
    Actual =:= Expect.
    