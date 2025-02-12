-module(prop_ecron_spec).

-include_lib("proper/include/proper.hrl").

-export([datetime/0, month/0]).
-export([standard_spec/0, extend_spec/0, integer_spec/2, extend_spec/2]).
-export([maybe_error_spec/0]).
-export([crontab_spec/0]).

datetime() ->
    {{year(), month(), day()}, {hour(), minute(), second()}}.

year() -> range(0, 4000).
month() -> range(1, 12).
day() -> range(1, 31).
hour() -> range(0, 23).
minute() -> range(0, 59).
second() -> range(0, 59).

standard_spec() ->
    [
        integer_spec(0, 59),
        integer_spec(0, 23),
        integer_spec(1, 31),
        oneof([integer_spec(1, 12), alphabet_spec(month, 1, 12)]),
        oneof([integer_spec(0, 6), alphabet_spec(day_of_week, 0, 6)])
    ].

extend_spec() ->
    [
        integer_spec(0, 59),
        integer_spec(0, 59),
        integer_spec(0, 23),
        integer_spec(1, 31),
        oneof([integer_spec(1, 12), alphabet_spec(month, 1, 12)]),
        oneof([integer_spec(0, 6), alphabet_spec(day_of_week, 0, 6)])
    ].

extend_spec(Min, Max) ->
    [
        integer_spec(0, 59),
        integer_spec(0, 59),
        integer_spec(Min, Max),
        integer_spec(1, 31),
        oneof([integer_spec(1, 12), alphabet_spec(month, 1, 12)]),
        oneof([integer_spec(0, 6), alphabet_spec(day_of_week, 0, 6)])
    ].

maybe_error_spec() ->
    [
        %% second
        error_integer_spec(0, 100),
        %% minute
        error_integer_spec(50, 80),
        %% hour
        error_integer_spec(20, 30),
        %% day_of_month
        error_integer_spec(20, 40),
        %% month
        error_integer_spec(10, 20),
        %% day_of_week
        error_integer_spec(5, 10)
    ].

integer_spec(Min, Max) ->
    frequency([
        %% "*"
        {1, "*"},
        %% "*/Step"
        {1, {'*/Step', general, Min, Max, range(1, 31)}},
        {8, ?SIZED(S, integer_spec(S, Min, Max))}
    ]).

integer_spec(S, Min, Max) ->
    oneof([
        %% 1
        {'Integer', general, range(Min, Max)},
        %% 1-5
        'Min-Max'(general, range(Min, Max), Max),
        %% 1-5/2
        'Min-Max/Step'(general, range(Min, Max), Max, range(1, 31)),
        %% 10/2
        {'Min/Step', general, range(Min, Max), Max, range(1, 31)},
        %% 1,2-6/2...
        ?LAZY({list, general, [integer_spec(S - 1, Min, Max), integer_spec(S - 2, Min, Max)]})
    ]).

error_integer_spec(Min, Max) ->
    ?SIZED(S, error_integer_spec(S, Min, Max)).

error_integer_spec(S, Min, Max) ->
    oneof([
        %% 1
        {'Integer', general, range(Min, Max)},
        %% 1-5
        'Min-Max'(general, range(Min, Max), Max),
        %% 1-5/2
        'Min-Max/Step'(general, range(Min, Max), Max, range(1, 31)),
        %% 1,2-6/2...
        ?LAZY(
            {list, general, [
                error_integer_spec(S - 1, Min, Max), error_integer_spec(S - 2, Min, Max)
            ]}
        )
    ]).

alphabet_spec(Type, Min, Max) ->
    ?SIZED(S, alphabet_spec(S, Type, Min, Max)).

alphabet_spec(S, Type, Min, Max) ->
    oneof([
        %% 1
        {'Integer', Type, range(Min, Max)},
        %% 1-5
        'Min-Max'(Type, range(Min, Max), Max),
        %% 1-5/2
        'Min-Max/Step'(Type, range(Min, Max), Max, range(1, 31)),
        %% 10/2
        {'Min/Step', Type, range(Min, Max), Max, range(1, 31)},
        %% 1,2-6/2...
        ?LAZY(
            {list, Type, [
                alphabet_spec(S - 1, Type, Min, Max), alphabet_spec(S - 2, Type, Min, Max)
            ]}
        )
    ]).

crontab_spec() ->
    oneof([
        extend_spec_1(),
        spec_1(),
        map_spec()
    ]).

%%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%%
'Min-Max'(Type, Min, MaxLimit) ->
    ?LET({MinF, MaxLimitF}, {Min, MaxLimit}, {'Min-Max', Type, MinF, range(MinF, MaxLimitF)}).
'Min-Max/Step'(Type, Min, MaxLimit, Step) ->
    ?LET(
        {MinF, MaxLimitF, StepF},
        {Min, MaxLimit, Step},
        {'Min-Max/Step', Type, MinF, range(MinF, MaxLimitF), StepF}
    ).

map_spec() ->
    ?LET(Spec, extend_spec_2(), prop_ecron:cron_spec_to_map(Spec)).

extend_spec_1() ->
    ?LET(Spec, extend_spec_2(), prop_ecron:spec_to_str(Spec)).

extend_spec_2() ->
    ?SUCHTHAT(Spec, extend_spec(), prop_ecron:check_day_of_month(Spec)).

spec_1() ->
    ?LET(Spec, spec_2(), prop_ecron:spec_to_str(Spec)).

spec_2() ->
    ?SUCHTHAT(Spec, standard_spec(), prop_ecron:check_day_of_month(Spec)).
