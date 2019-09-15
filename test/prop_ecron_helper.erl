-module(prop_ecron_helper).

-include_lib("proper/include/proper.hrl").
-export([spec/0, extend_spec/0, integer_spec/2]).
-export([maybe_error_spec/0]).
-export([spec_to_str/1, cron_spec_to_map/1]).
-export([unzip/1]).
-export([check_day_of_month/1]).
-export([field_to_extend/1]).
-export([get_max_day_of_months/1]).
-export([to_now_datetime/2]).

spec() ->
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

maybe_error_spec() ->
    [
        error_integer_spec(0, 100), %% second
        error_integer_spec(50, 80), %% minute
        error_integer_spec(20, 30), %% hour
        error_integer_spec(20, 40), %% day_of_month
        error_integer_spec(10, 20), %% month
        error_integer_spec(5, 10)  %% day_of_week
    ].

integer_spec(Min, Max) ->
    frequency([
        {1, "*"}, %% "*"
        {1, {'*/Step', general, Min, Max, range(1, 100)}}, %% "*/Step"
        {8, ?SIZED(S, integer_spec(S, Min, Max))}]).

integer_spec(S, Min, Max) ->
    oneof([
        {'Integer', general, range(Min, Max)}, %% 1
        'Min-Max'(general, range(Min, Max), Max), %% 1-5
        'Min-Max/Step'(general, range(Min, Max), Max, range(1, 100)), %% 1-5/2
        {'Min/Step', general, range(Min, Max), Max, range(1, 100)}, %% 10/2
        ?LAZY({list, general, [integer_spec(S - 1, Min, Max), integer_spec(S - 2, Min, Max)]}) %% 1,2-6/2...
    ]).

error_integer_spec(Min, Max) ->
        ?SIZED(S, error_integer_spec(S, Min, Max)).

error_integer_spec(S, Min, Max) ->
    oneof([
        {'Integer', general, range(Min, Max)}, %% 1
        'Min-Max'(general, range(Min, Max), Max), %% 1-5
        'Min-Max/Step'(general, range(Min, Max), Max, range(1, 100)), %% 1-5/2
        ?LAZY({list, general, [error_integer_spec(S - 1, Min, Max), error_integer_spec(S - 2, Min, Max)]}) %% 1,2-6/2...
    ]).

alphabet_spec(Type, Min, Max) ->
    ?SIZED(S, alphabet_cron_spec(S, Type, Min, Max)).

alphabet_cron_spec(S, Type, Min, Max) ->
    oneof([
        {'Integer', Type, range(Min, Max)}, %% 1
        'Min-Max'(Type, range(Min, Max), Max), %% 1-5
        'Min-Max/Step'(Type, range(Min, Max), Max, range(1, 100)), %% 1-5/2
        {'Min/Step', Type, range(Min, Max), Max, range(1, 100)}, %% 10/2
        ?LAZY({list, Type, [alphabet_cron_spec(S - 1, Type, Min, Max), alphabet_cron_spec(S - 2, Type, Min, Max)]}) %% 1,2-6/2...
    ]).

spec_to_str(Spec) ->
    Format = string:join(lists:duplicate(length(Spec), "~s"), " "),
    SpecTmp = [field_spec_to_str(S) || S <- Spec],
    lists:flatten(io_lib:format(Format, SpecTmp)).

cron_spec_to_map(Spec) ->
    [Second, Minute, Hour, Day, Month, Week] = [field_to_extend(S) || S <- Spec],
    #{ second => Second,
        minute => Minute,
        hour => Hour,
        day_of_month => Day,
        month => Month,
        day_of_week => Week
    }.

field_spec_to_str("*") -> "*";
field_spec_to_str({'*/Step', _Type, _MinLimit, _MaxLimit, Step}) -> "*/" ++ integer_to_list(Step);
field_spec_to_str({'Integer', Type, Int}) -> int_to_str(Type, Int);
field_spec_to_str({'Min-Max', Type, Min, Max}) -> int_to_str(Type, Min) ++ "-" ++ int_to_str(Type, Max);
field_spec_to_str({'Min/Step', Type, Min, _MaxLimit, Step}) -> int_to_str(Type, Min) ++ "/" ++ integer_to_list(Step);
field_spec_to_str({'Min-Max/Step', Type, Min, Max, Step}) ->
    int_to_str(Type, Min) ++ "-" ++ int_to_str(Type, Max) ++ "/" ++ integer_to_list(Step);
field_spec_to_str({list, _Type, List}) -> string:join(lists:map(fun field_spec_to_str/1, List), ",").

unzip(List) -> lists:usort(unzip_list(List, [])).
unzip_list([], Acc) -> Acc;
unzip_list([H | T], Acc) when is_integer(H) -> unzip_list(T, [H | Acc]);
unzip_list([{Min, Max} | T], Acc) -> unzip_list(T, lists:seq(Min, Max) ++ Acc).

check_day_of_month([_M, _H, _DOM, _Month, _DOW] = T) -> check_day_of_month(["0" | T]);
check_day_of_month([_S, _M, _H, "*", _Month, _DOW]) -> true;
check_day_of_month([_S, _M, _H, _DOM, "*", _DOW]) -> true;
check_day_of_month([_S, _M, _H, DOM, Month, _DOW]) ->
    DOMExtend = unzip(field_to_extend(DOM)),
    MonthExtend = unzip(field_to_extend(Month)),
    ecron:get_max_day_of_months(MonthExtend) >= lists:max(DOMExtend).

field_to_extend("*") -> '*';
field_to_extend({'*/Step', _Type, MinLimit, MaxLimit, Step}) -> ecron:zip(lists:seq(MinLimit, MaxLimit, Step));
field_to_extend({'Integer', _Type, Int}) -> [Int];
field_to_extend({'Min-Max', _Type, Min, Max}) -> ecron:zip(lists:seq(Min, Max));
field_to_extend({'Min/Step', _Type, Min, MaxLimit, Step}) -> ecron:zip(lists:seq(Min, MaxLimit, Step));
field_to_extend({'Min-Max/Step', _Type, Min, Max, Step}) -> ecron:zip(lists:seq(Min, Max, Step));
field_to_extend({list, _Type, List}) -> ecron:zip(unzip(lists:flatten([field_to_extend(L) || L <- List]))).

get_max_day_of_months(List) ->
    lists:max(lists:map(fun max_day_of_month/1, List)).

to_now_datetime(_, unlimited) -> unlimited;
to_now_datetime(utc, Time) -> calendar:system_time_to_universal_time(calendar:rfc3339_to_system_time(Time), second);
to_now_datetime(local, Time) -> calendar:system_time_to_local_time(calendar:rfc3339_to_system_time(Time), second).

%%%%%%%%%%%%%%%%%
%%% Internal  %%%
%%%%%%%%%%%%%%%%%
'Min-Max'(Type, Min, MaxLimit) ->
    ?LET({MinF, MaxLimitF}, {Min, MaxLimit}, {'Min-Max', Type, MinF, range(MinF, MaxLimitF)}).
'Min-Max/Step'(Type, Min, MaxLimit, Step) ->
    ?LET({MinF, MaxLimitF, StepF}, {Min, MaxLimit, Step}, {'Min-Max/Step', Type, MinF, range(MinF, MaxLimitF), StepF}).

int_to_str(day_of_week, 0) -> "sun";
int_to_str(day_of_week, 1) -> "mon";
int_to_str(day_of_week, 2) -> "tue";
int_to_str(day_of_week, 3) -> "wed";
int_to_str(day_of_week, 4) -> "thu";
int_to_str(day_of_week, 5) -> "fir";
int_to_str(day_of_week, 6) -> "sat";
int_to_str(month, 1) -> "jan";
int_to_str(month, 2) -> "feb";
int_to_str(month, 3) -> "mar";
int_to_str(month, 4) -> "apr";
int_to_str(month, 5) -> "may";
int_to_str(month, 6) -> "jun";
int_to_str(month, 7) -> "jul";
int_to_str(month, 8) -> "aug";
int_to_str(month, 9) -> "sep";
int_to_str(month, 10) -> "oct";
int_to_str(month, 11) -> "nov";
int_to_str(month, 12) -> "dec";
int_to_str(general, Int) -> integer_to_list(Int).

max_day_of_month(1) -> 31;
max_day_of_month(2) -> 29;
max_day_of_month(3) -> 31;
max_day_of_month(4) -> 30;
max_day_of_month(5) -> 31;
max_day_of_month(6) -> 30;
max_day_of_month(7) -> 31;
max_day_of_month(8) -> 31;
max_day_of_month(9) -> 30;
max_day_of_month(10) -> 31;
max_day_of_month(11) -> 30;
max_day_of_month(12) -> 31.
