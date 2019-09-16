-module(prop_ecron_job).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3, precondition/2, postcondition/3]).
-export([prop_job_worker/1, prop_job_worker/0]).

-export([
    add_cron_new/3, add_cron_existing/3, add_cron_new/4, add_cron_existing/4,
    add_every_new/3, add_every_existing/3, add_every_new/4, add_every_existing/4,
    delete_existing/1, delete_unknown/1,
    deactivate_unknown/1, deactivate_existing/1,
    activate_unknown/1, activate_existing/1,
    statistic_unknown/1, statistic_existing/1, statistic_all/0,
    reload_all/0
]).

-import(prop_ecron_spec, [crontab_spec/0]).
-import(prop_ecron_helper, [spec_to_str/1, cron_spec_to_map/1]).
-import(prop_ecron_helper, [check_day_of_month/1, to_now_datetime/2]).
-define(MAX_TIMEOUT, 4294967). %% (16#ffffffff div 1000) 49.71 days.

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_job_worker(doc) -> "job stateful faield";
prop_job_worker(opts) -> [{numtests, 7000}].
prop_job_worker() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            application:set_env(ecron, jobs, []),
            application:ensure_all_started(ecron),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            [begin supervisor:terminate_child(ecron_sup, Pid) end ||
                {_, Pid, worker, [ecron_job]} <- supervisor:which_children(ecron_sup)],
            ets:delete_all_objects(ecron),
            ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                [History, State, Result]),
                aggregate(command_names(Cmds), Result =:= ok))
        end).

%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    #{}.

%% @doc List of possible commands to run against the system
command(State) ->
    Empty = State =/= #{},
    frequency(
        [
            {5, {call, ?MODULE, add_cron_new, [new_name(State), crontab_spec(), mfa()]}},
            {5, {call, ?MODULE, add_cron_new, [new_name(State), crontab_spec(), mfa(), datetime()]}},
            {4, {call, ?MODULE, add_every_new, [new_name(State), range(1, ?MAX_TIMEOUT), mfa()]}},
            {4, {call, ?MODULE, add_every_new, [new_name(State), range(1, ?MAX_TIMEOUT), mfa(), datetime()]}},
            {1, {call, ?MODULE, delete_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, deactivate_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, activate_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, statistic_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, reload_all, []}},
            {1, {call, ?MODULE, statistic_all, []}}
        ] ++
            [{1, {call, ?MODULE, add_cron_existing, [exist_name(State), crontab_spec(), mfa()]}} || Empty] ++
            [{1, {call, ?MODULE, add_cron_existing, [exist_name(State), crontab_spec(), mfa(), datetime()]}} || Empty] ++
            [{1, {call, ?MODULE, add_every_existing, [exist_name(State), range(1, ?MAX_TIMEOUT), mfa()]}} || Empty] ++
            [{1, {call, ?MODULE, add_every_existing, [exist_name(State), range(1, ?MAX_TIMEOUT), mfa(), datetime()]}} || Empty] ++
            [{4, {call, ?MODULE, delete_existing, [exist_name(State)]}} || Empty] ++
            [{8, {call, ?MODULE, deactivate_existing, [exist_name(State)]}} || Empty] ++
            [{8, {call, ?MODULE, activate_existing, [exist_name(State)]}} || Empty] ++
            [{8, {call, ?MODULE, statistic_existing, [exist_name(State)]}} || Empty]
    ).

%% @doc Determines whether a command should be valid under the
%% current state.

precondition(State, {call, _Mod, add_cron_new, [Name | _]}) -> not_in(Name, State);
precondition(State, {call, _Mod, add_cron_existing, [Name | _]}) -> in(Name, State);

precondition(State, {call, _Mod, add_every_new, [Name | _]}) -> not_in(Name, State);
precondition(State, {call, _Mod, add_every_existing, [Name | _]}) -> in(Name, State);

precondition(State, {call, _Mod, delete_unknown, [Name | _]}) -> not_in(Name, State);
precondition(State, {call, _Mod, delete_existing, [Name | _]}) -> in(Name, State);

precondition(State, {call, _Mod, deactivate_unknown, [Name | _]}) -> not_in(Name, State);
precondition(State, {call, _Mod, deactivate_existing, [Name | _]}) -> in(Name, State);

precondition(State, {call, _Mod, activate_unknown, [Name | _]}) -> not_in(Name, State);
precondition(State, {call, _Mod, activate_existing, [Name | _]}) -> in(Name, State);

precondition(State, {call, _Mod, statistic_unknown, [Name | _]}) -> not_in(Name, State);
precondition(State, {call, _Mod, statistic_existing, [Name | _]}) -> in(Name, State);

precondition(_State, {call, _Mod, _Fun, _Args}) -> true.

%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the ecron) makes sense.

postcondition(_State, {call, _Mod, add_cron_new, [_Name | _]}, Res) ->
    is_pid(element(2, Res));
postcondition(_State, {call, _Mod, add_cron_existing, [_Name | _]}, Res) ->
    Res =:= {error, already_exist};
postcondition(_State, {call, _Mod, add_every_new, [_Name | _]}, Res) ->
    is_pid(element(2, Res));
postcondition(_State, {call, _Mod, add_every_existing, [_Name | _]}, Res) ->
    Res =:= {error, already_exist};
postcondition(_State, {call, _Mod, delete_unknown, [_Name | _]}, Res) ->
    Res =:= {error, not_found};
postcondition(_State, {call, _Mod, delete_existing, [_Name | _]}, Res) ->
    Res =:= ok;
postcondition(_State, {call, _Mod, deactivate_unknown, [_Name | _]}, Res) ->
    Res =:= {error, not_found};
postcondition(_State, {call, _Mod, deactivate_existing, [_Name | _]}, Res) ->
    Res =:= ok;
postcondition(_State, {call, _Mod, activate_unknown, [_Name | _]}, Res) ->
    Res =:= {error, not_found};
postcondition(_State, {call, _Mod, activate_existing, [_Name | _]}, Res) ->
    Res =:= ok;
postcondition(_State, {call, _Mod, statistic_unknown, [_Name | _]}, Res) ->
    Res =:= {error, not_found};
postcondition(State, {call, _Mod, statistic_existing, [Name | _]}, Res) ->
    valid_statistic(State, Name, Res);
postcondition(State, {call, _Mod, statistic_all, []}, Res) ->
    erlang:length(Res) =:= maps:size(State);
postcondition(_State, {call, _Mod, reload_all, []}, Res) ->
    Res =:= ok.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, Res, {call, _Mod, add_cron_new, [Name | _] = Args}) ->
    State#{Name => #{cron => new_cron(Args), worker => Res}};
next_state(State, Res, {call, _Mod, add_every_new, [Name | _] = Args}) ->
    State#{Name => #{cron => new_every(Args), worker => Res}};
next_state(State, _Res, {call, _Mod, delete_existing, [Name]}) -> maps:remove(Name, State);
next_state(State, _Res, {call, _Mod, deactivate_existing, [_Name]}) -> State;
next_state(State, _Res, {call, _Mod, activate_existing, [_Name]}) -> State;
next_state(State, _Res, {call, _Mod, statistic_existing, [_Name]}) -> State;
next_state(State, _Res, {call, _Mod, _Fun, _Args}) -> State.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

new_cron([Name, Spec, MFA]) ->
    new_cron([Name, Spec, MFA, {unlimited, unlimited}]);
new_cron([Name, Spec, MFA, {Start, End}]) ->
    {ok, Type, Crontab} = ecron:parse_spec(Spec),
    #{type => Type, name => Name,
        crontab => Crontab, mfa => MFA,
        start_time => Start, end_time => End
    }.

new_every([Name, Second, MFA]) ->
    new_every([Name, Second, MFA, {unlimited, unlimited}]);
new_every([Name, Second, MFA, {Start, End}]) ->
    #{type => every, name => Name,
        crontab => Second * 1000, start_time => Start,
        end_time => End, mfa => MFA
    }.

add_cron_new(Name, Spec, MFA) -> ecron:add(Name, Spec, MFA).
add_cron_existing(Name, Spec, MFA) -> ecron:add(Name, Spec, MFA).
add_cron_new(Name, Spec, MFA, {Start, End}) -> ecron:add(Name, Spec, MFA, Start, End).
add_cron_existing(Name, Spec, MFA, {Start, End}) -> ecron:add(Name, Spec, MFA, Start, End).

add_every_new(Name, Ms, MFA) -> ecron:add(Name, Ms, MFA).
add_every_existing(Name, Ms, MFA) -> ecron:add(Name, Ms, MFA).
add_every_new(Name, Ms, MFA, {Start, End}) -> ecron:add(Name, Ms, MFA, Start, End).
add_every_existing(Name, Ms, MFA, {Start, End}) -> ecron:add(Name, Ms, MFA, Start, End).

delete_existing(Name) -> ecron:delete(Name).
delete_unknown(Name) -> ecron:delete(Name).

deactivate_unknown(Name) -> ecron:deactivate(Name).
deactivate_existing(Name) -> ecron:deactivate(Name).

activate_unknown(Name) -> ecron:activate(Name).
activate_existing(Name) -> ecron:activate(Name).

statistic_unknown(Name) -> ecron:statistic(Name).
statistic_existing(Name) -> ecron:statistic(Name).
statistic_all() -> ecron:statistic().

reload_all() -> ecron:reload().

valid_statistic(State, Name, {ok, Res}) ->
    case maps:find(Name, State) of
        error -> false;
        {ok, #{cron := #{type := Type, crontab := CrontabSpec}}} ->
            #{ecron := #{crontab := Cron, start_time := StartTime, end_time := EndTime},
                failed := Failed, next := Next, ok := Ok,
                results := Results, run_microsecond := RunMs,
                status := Status, time_zone := TimeZone, worker := Worker
            } = Res,
            case Cron =:= CrontabSpec andalso
                (Worker =:= undefined andalso lists:member(Status, [already_ended, deactivate])) orelse
                (is_pid(Worker) andalso not lists:member(Status, [already_ended, deactivate])) andalso
                    Failed =:= 0 andalso
                    Ok >= 0 andalso
                    length(Results) =< 20 andalso
                    length(Results) =:= length(RunMs)
            of
                true ->
                    StartTime1 = to_now_datetime(TimeZone, StartTime),
                    EndTime1 = to_now_datetime(TimeZone, EndTime),
                    case Type of
                        cron ->
                            prop_ecron_predict_datetime:check_cron_result(CrontabSpec,
                                TimeZone, StartTime1, EndTime1, Next);
                        every when Next =:= [] -> true;
                        every ->
                            Second = CrontabSpec div 1000,
                            Now = calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(
                                    to_now_datetime(TimeZone,
                                        hd(Next))) - Second),
                            prop_ecron_predict_datetime:check_every_result(Second,
                                TimeZone, StartTime1, EndTime1, Now, Next)
                    end;
                false -> false
            end
    end.

in(Name, State) -> maps:is_key(Name, State).
not_in(Name, State) -> not in(Name, State).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

new_name(Map) when Map =:= #{} -> atom();
new_name(Map) -> ?SUCHTHAT(L, atom(), not_in(L, Map)).

exist_name(Map) -> oneof(maps:keys(Map)).

mfa() ->
    {io_lib, format, ["~p", [range(1000, 2000)]]}.

datetime() ->
    {Year, _, _} = erlang:date(),
    Start =
        ?SUCHTHAT({DateS, _TimeS},
            {{range(Year - 100, Year), range(1, 12), range(1, 31)}, {range(0, 23), range(0, 59), range(0, 59)}},
            calendar:valid_date(DateS)
        ),
    End =
        ?SUCHTHAT({DateS, _TimeS},
            {{range(Year + 1, Year + 100), range(1, 12), range(1, 31)}, {range(0, 23), range(0, 59), range(0, 59)}},
            calendar:valid_date(DateS)
        ),
    {Start, End}.
