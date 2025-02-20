-module(prop_ecron_server).
-include_lib("proper/include/proper.hrl").
-include_lib("ecron/include/ecron.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3, precondition/2, postcondition/3]).
-export([prop_server/1, prop_server/0]).

-export([
    add_cron_new/3,
    add_cron_existing/3,
    add_cron_new/4,
    add_cron_existing/4,
    add_with_count/3,
    add_with_datetime/3,
    add_every_new/3,
    add_every_existing/3,
    add_every_new/4,
    add_every_existing/4,
    delete_existing/1,
    delete_unknown/1,
    deactivate_unknown/1,
    deactivate_existing/1,
    activate_unknown/1,
    activate_existing/1,
    statistic_unknown/1,
    statistic_existing/1,
    statistic_all/0,
    reload/0
]).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_server(doc) -> "job stateful faield";
prop_server(opts) -> [{numtests, 7000}].
prop_server() ->
    ?FORALL(
        Cmds,
        commands(?MODULE),
        begin
            application:set_env(ecron, local_jobs, []),
            application:set_env(ecron, global_jobs, []),
            application:ensure_all_started(ecron),
            ecron:clear(),
            {History, State, Result} = run_commands(?MODULE, Cmds),
            ets:delete_all_objects(?LocalJob),
            ?WHENFAIL(
                io:format(
                    "History: ~p\nState: ~p\nResult: ~p\n",
                    [History, State, Result]
                ),
                aggregate(command_names(Cmds), Result =:= ok)
            )
        end
    ).

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
            {5,
                {call, ?MODULE, add_cron_new, [
                    new_name(State), prop_ecron_spec:crontab_spec(), mfa()
                ]}},
            {5,
                {call, ?MODULE, add_cron_new, [
                    new_name(State), prop_ecron_spec:crontab_spec(), mfa(), datetime()
                ]}},
            {4, {call, ?MODULE, add_every_new, [new_name(State), range(1, ?MAX_TIMEOUT), mfa()]}},
            {4,
                {call, ?MODULE, add_every_new, [
                    new_name(State), range(1, ?MAX_TIMEOUT), mfa(), datetime()
                ]}},
            {4,
                {call, ?MODULE, add_with_count, [
                    prop_ecron_spec:crontab_spec(), mfa(), range(100, 1000)
                ]}},
            {4,
                {call, ?MODULE, add_with_datetime, [
                    prop_ecron_spec:crontab_spec(), mfa(), datetime()
                ]}},
            {1, {call, ?MODULE, delete_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, deactivate_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, activate_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, statistic_unknown, [new_name(State)]}},
            {1, {call, ?MODULE, statistic_all, []}},
            {1, {call, ?MODULE, reload, []}}
        ] ++
            [
                {1,
                    {call, ?MODULE, add_cron_existing, [
                        exist_name(State), prop_ecron_spec:crontab_spec(), mfa()
                    ]}}
             || Empty
            ] ++
            [
                {1,
                    {call, ?MODULE, add_cron_existing, [
                        exist_name(State), prop_ecron_spec:crontab_spec(), mfa(), datetime()
                    ]}}
             || Empty
            ] ++
            [
                {1,
                    {call, ?MODULE, add_every_existing, [
                        exist_name(State), range(1, ?MAX_TIMEOUT), mfa()
                    ]}}
             || Empty
            ] ++
            [
                {1,
                    {call, ?MODULE, add_every_existing, [
                        exist_name(State), range(1, ?MAX_TIMEOUT), mfa(), datetime()
                    ]}}
             || Empty
            ] ++
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

postcondition(_State, {call, _Mod, add_cron_new, [_Name | _] = Args}, Res) ->
    check_add_new(Args, Res);
postcondition(_State, {call, _Mod, add_with_count, [_ | _] = Args}, Res) ->
    check_add_with_limit(Args, Res);
postcondition(_State, {call, _Mod, add_with_datetime, [_ | _] = Args}, Res) ->
    check_add_with_limit(Args, Res);
postcondition(_State, {call, _Mod, add_cron_existing, [_Name | _]}, Res) ->
    element(1, Res) =:= error andalso
        (element(2, Res) =:= already_exist orelse element(2, Res) =:= invalid_time);
postcondition(_State, {call, _Mod, add_every_new, [_Name | _] = Args}, Res) ->
    check_add_new(Args, Res);
postcondition(_State, {call, _Mod, add_every_existing, [_Name | _]}, Res) ->
    Res =:= {error, already_exist};
postcondition(_State, {call, _Mod, delete_unknown, [_Name | _]}, Res) ->
    Res =:= ok;
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
    Res =:= [];
postcondition(State, {call, _Mod, statistic_existing, [Name | _]}, Res) ->
    valid_statistic(State, Name, Res);
postcondition(_State, {call, _Mod, reload, []}, Res) ->
    Res =:= ok;
postcondition(State, {call, _Mod, statistic_all, []}, Res) ->
    erlang:length(Res) =:= maps:size(State).

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.

next_state(State, Res, {call, _Mod, add_cron_new, [Name, _Spec, _MFA] = Args}) ->
    State#{Name => #{cron => new_cron(Args), worker => Res}};
next_state(State, Res, {call, _Mod, add_cron_new, [Name, Spec, _MFA, {Start, End}] = Args}) ->
    case is_valid_time(Spec, Start, End) of
        false -> State;
        true -> State#{Name => #{cron => new_cron(Args), worker => Res}}
    end;
next_state(State, Res, {call, _Mod, add_with_count, [Spec, MFA, _Count]}) ->
    Name = make_ref(),
    State#{Name => #{cron => new_cron([Name, Spec, MFA]), worker => Res}};
next_state(State, Res, {call, _Mod, add_with_datetime, [Spec, MFA, {Start, End}]}) ->
    case is_valid_time(Spec, Start, End) of
        false ->
            State;
        true ->
            Name = make_ref(),
            State#{Name => #{cron => new_cron([Name, Spec, MFA, {Start, End}]), worker => Res}}
    end;
next_state(State, Res, {call, _Mod, add_every_new, [Name, _Spec, _MFA] = Args}) ->
    State#{Name => #{cron => new_every(Args), worker => Res}};
next_state(State, Res, {call, _Mod, add_every_new, [Name, Spec, _MFA, {Start, End}] = Args}) ->
    case is_valid_time(Spec, Start, End) of
        false -> State;
        true -> State#{Name => #{cron => new_every(Args), worker => Res}}
    end;
next_state(State, _Res, {call, _Mod, delete_existing, [Name]}) ->
    maps:remove(Name, State);
next_state(State, _Res, {call, _Mod, deactivate_existing, [_Name]}) ->
    State;
next_state(State, _Res, {call, _Mod, activate_existing, [_Name]}) ->
    State;
next_state(State, _Res, {call, _Mod, statistic_existing, [_Name]}) ->
    State;
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    State.

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

new_cron([Name, Spec, MFA]) ->
    new_cron([Name, Spec, MFA, {unlimited, unlimited}]);
new_cron([Name, Spec, MFA, {Start, End}]) ->
    {ok, Type, Crontab} = ecron_spec:parse_spec(Spec),
    #{
        type => Type,
        name => Name,
        crontab => Crontab,
        mfa => MFA,
        start_time => Start,
        end_time => End
    }.

new_every([Name, Second, MFA]) ->
    new_every([Name, Second, MFA, {unlimited, unlimited}]);
new_every([Name, Second, MFA, {Start, End}]) ->
    #{
        type => every,
        name => Name,
        crontab => Second,
        start_time => Start,
        end_time => End,
        mfa => MFA
    }.
check_add_new([Name, _Spec, _MFA | _], {ok, Name}) ->
    true;
check_add_new([_Name, Spec, _MFA, {StartTime, EndTime}], {error, invalid_time, _}) ->
    not is_valid_time(Spec, StartTime, EndTime).

check_add_with_limit([_Spec, _MFA | _], {ok, _Name}) ->
    true;
check_add_with_limit([Spec, _MFA, {StartTime, EndTime}], {error, invalid_time, _}) ->
    not is_valid_time(Spec, StartTime, EndTime).

is_valid_time(Spec, {SH, SM, SS} = StartTime, {EH, EM, ES} = EndTime) ->
    Start = SH * 3600 + SM * 60 + SS,
    End = EH * 3600 + EM * 60 + ES,
    case End > Start of
        false ->
            false;
        true ->
            case ecron_spec:parse_spec(Spec) of
                {ok, cron, Job} ->
                    #{hour := HourSpec, minute := MinuteSpec, second := SecondSpec} = Job,
                    MaxHour = parse_max(23, HourSpec),
                    MaxMinute = parse_max(59, MinuteSpec),
                    MaxSecond = parse_max(59, SecondSpec),
                    MaxSpecTime = MaxHour * 3600 + MaxMinute * 60 + MaxSecond,
                    MinHour = parse_min(0, HourSpec),
                    MinMinute = parse_min(0, MinuteSpec),
                    MinSecond = parse_min(0, SecondSpec),
                    MinSpecTime = MinHour * 3600 + MinMinute * 60 + MinSecond,
                    case MaxSpecTime >= Start andalso MinSpecTime =< End of
                        false ->
                            false;
                        true ->
                            case
                                ecron:predict_datetime(
                                    #{type => cron, crontab => Job}, 1, StartTime, EndTime
                                )
                            of
                                {ok, [_]} -> true;
                                _ -> false
                            end
                    end;
                _ ->
                    true
            end
    end.

parse_max(Max, '*') ->
    Max;
parse_max(_DefaultMax, List) ->
    case lists:last(List) of
        {_, Max} -> Max;
        Max -> Max
    end.

parse_min(Min, '*') -> Min;
parse_min(_DefaultMin, [{Min, _} | _]) -> Min;
parse_min(_DefaultMin, [Min | _]) -> Min.

add_cron_new(Name, Spec, MFA) -> ecron:create(Name, Spec, MFA).
add_cron_existing(Name, Spec, MFA) -> ecron:create(Name, Spec, MFA).
add_cron_new(Name, Spec, MFA, {Start, End}) ->
    ecron:create(Name, Spec, MFA, #{start_time => Start, end_time => End}).
add_cron_existing(Name, Spec, MFA, {Start, End}) ->
    ecron:create(Name, Spec, MFA, #{start_time => Start, end_time => End}).

add_with_count(Spec, MFA, Count) -> ecron:create(make_ref(), Spec, MFA, #{max_count => Count}).
add_with_datetime(Spec, MFA, {Start, End}) ->
    ecron:create(make_ref(), Spec, MFA, #{start_time => Start, end_time => End}).

add_every_new(Name, Ms, MFA) -> ecron:create(Name, Ms, MFA).
add_every_existing(Name, Ms, MFA) -> ecron:create(Name, Ms, MFA).
add_every_new(Name, Ms, MFA, {Start, End}) ->
    ecron:create(Name, Ms, MFA, #{start_time => Start, end_time => End}).
add_every_existing(Name, Ms, MFA, {Start, End}) ->
    ecron:create(Name, Ms, MFA, #{start_time => Start, end_time => End}).

delete_existing(Name) -> ecron:delete(Name).
delete_unknown(Name) -> ecron:delete(Name).

deactivate_unknown(Name) -> ecron:deactivate(Name).
deactivate_existing(Name) -> ecron:deactivate(Name).

activate_unknown(Name) -> ecron:activate(Name).
activate_existing(Name) -> ecron:activate(Name).

statistic_unknown(Name) -> ecron:statistic(Name).
statistic_existing(Name) -> ecron:statistic(Name).
statistic_all() -> ecron:statistic().
reload() -> ecron:reload().

valid_statistic(State, Name, [Res]) ->
    case maps:find(Name, State) of
        error ->
            false;
        {ok, #{cron := #{type := Type, crontab := CrontabSpec, mfa := MFAExpect}}} ->
            #{
                crontab := Cron,
                start_time := StartTime,
                end_time := EndTime,
                crashed := Crashed,
                next := Next,
                ok := Ok,
                mfa := MFA,
                results := Results,
                run_microsecond := RunMs
            } = Res,
            case
                Cron =:= CrontabSpec andalso
                    Crashed =:= 0 andalso
                    Ok >= 0 andalso
                    MFAExpect =:= MFA andalso
                    length(Results) =< 20 andalso
                    length(Results) =:= length(RunMs)
            of
                true ->
                    case Type of
                        cron ->
                            prop_ecron:check_cron_result(CrontabSpec, StartTime, EndTime, Next);
                        every ->
                            prop_ecron:check_every_result(
                                CrontabSpec, StartTime, EndTime, local, Next
                            )
                    end;
                false ->
                    false
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
    Start = {range(0, 12), range(0, 59), range(0, 59)},
    End = {range(13, 23), range(0, 59), range(0, 59)},
    {Start, End}.
