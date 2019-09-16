-module(prop_ecron_application).
-include_lib("proper/include/proper.hrl").
%% API
-export([prop_application/0, prop_application/1]).
-export([prop_application_error_config/0, prop_application_error_config/1]).

-import(prop_ecron_spec, [standard_spec/0, maybe_error_spec/0, extend_spec/0]).
-import(prop_ecron_helper, [check_day_of_month/1, spec_to_str/1]).

prop_application(doc) -> "ecron application load normal config failed";
prop_application(opts) -> [{numtests, 100}].
prop_application() ->
    ?FORALL({Spec, UseTime}, {standard_spec(), bool()},
        ?IMPLIES(check_day_of_month(Spec),
            begin
                error_logger:tty(false),
                application:stop(ecron),
                SpecStr = spec_to_str(Spec),
                Jobs =
                    case UseTime of
                        true -> [{noraml, SpecStr, {io_lib, format, ["~s", [SpecStr]]}, unlimited, unlimited}];
                        false -> [{normal, SpecStr, {io_lib, format, ["~s", SpecStr]}}]
                    end,
                TimeZone = application:get_env(ecron, time_zone, local),
                application:set_env(ecron, time_zone, utc),
                application:set_env(ecron, jobs, Jobs),
                Result = application:start(ecron),
                
                application:set_env(ecron, time_zone, TimeZone),
                application:set_env(ecron, jobs, []),
                
                error_logger:tty(true),
                ?WHENFAIL(
                    io:format("Start ~s Failed: ~p\n", [SpecStr, Result]),
                    Result =:= ok
                )
            end)
    ).

prop_application_error_config(doc) -> "ecron application load error config failed";
prop_application_error_config(opts) -> [{numtests, 100}].
prop_application_error_config() ->
    ?FORALL({Spec, UseTime}, {maybe_error_spec(), range(0, 3)},
        begin
            error_logger:tty(false),
            application:stop(ecron),
            SpecStr = spec_to_str(Spec),
            Jobs =
                case UseTime of
                    0 -> [{test, error_format}];
                    1 -> [{test, SpecStr, {io_lib, format, ["~s", SpecStr]}}];
                    2 -> [{test, "* * * * * *", {io_lib, format, ["~s", SpecStr]}, {2019, 12, 10}, unlimited}];
                    3 -> [{test, test, {io_lib, format, ["~s", SpecStr]}}]
                end,
            application:set_env(ecron, jobs, Jobs),
            Res =
                case application:start(ecron) of
                    {error, {"invaild_time: {{2019,12,10},unlimited}", _}} -> true;
                    {error, {"invaild_spec: test", _}} -> true;
                    {error, {Actual, _}} ->
                        {error, Field, Reason} = ecron:parse_spec(SpecStr),
                        Expect = lists:flatten(io_lib:format("~p: ~p", [Field, Reason])),
                        ?WHENFAIL(
                            io:format("Start ~s \nFailed: ~p\n ~p\n", [SpecStr, Actual, Expect]),
                            Actual =:= Expect orelse Actual =:= {test, error_format}
                        );
                    ok ->
                        true
                end,
            error_logger:tty(true),
            Res
        end).
