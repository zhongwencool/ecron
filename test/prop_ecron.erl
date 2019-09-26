-module(prop_ecron).

-include_lib("proper/include/proper.hrl").
-export([spec_to_str/1, cron_spec_to_map/1]).
-export([unzip/1]).
-export([check_day_of_month/1]).
-export([field_to_extend/1]).
-export([to_ms/1]).
-export([field_spec_to_str/1]).
-export([max_day_of_month/1]).
-export([check_cron_result/4]).
-export([check_every_result/4]).

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

check_cron_result(_DateSpec, _StartTime, _EndTime, []) -> true;
check_cron_result(DateSpec, StartTime, EndTime, [T1 | Rest]) ->
    T = to_ms(T1),
    DateTime = calendar:system_time_to_local_time(T, millisecond),
    case is_greater_than_or_equal(T, StartTime) andalso
        is_greater_than_or_equal(EndTime, T) andalso
        in_cron_range(DateSpec, DateTime) of
        true -> check_cron_result(DateSpec, StartTime, EndTime, Rest);
        false -> false
    end.

check_every_result(_Second, _Start, _End, []) -> true;
check_every_result(_Second, _Start, _End, [_]) -> true;
check_every_result(Second, Start, End, [T1, T2 | Rest]) ->
    T11 = to_ms(T1) div 1000,
    T22 = to_ms(T2) div 1000,
    case is_greater_than_or_equal(T11, Start) andalso
        is_greater_than_or_equal(End, T11) andalso
        (T22 - T11) =:= Second of
        true -> check_every_result(Second, Start, End, [T2 | Rest]);
        false -> false
    end.

field_spec_to_str("*") -> "*";
field_spec_to_str({'*/Step', _Type, _MinLimit, _MaxLimit, Step}) -> "*/" ++ integer_to_list(Step);
field_spec_to_str({'Integer', Type, Int}) -> int_to_str(Type, Int);
field_spec_to_str({'Min-Max', Type, Min, Max}) -> int_to_str(Type, Min) ++ "-" ++ int_to_str(Type, Max);
field_spec_to_str({'Min/Step', Type, Min, _MaxLimit, Step}) -> int_to_str(Type, Min) ++ "/" ++ integer_to_list(Step);
field_spec_to_str({'Min-Max/Step', Type, Min, Max, Step}) ->
    int_to_str(Type, Min) ++ "-" ++ int_to_str(Type, Max) ++ "/" ++ integer_to_list(Step);
field_spec_to_str({list, _Type, List}) -> string:join(lists:map(fun field_spec_to_str/1, List), ",").

unzip(undefined) -> [];
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

to_ms(unlimited) -> unlimited;
to_ms(Time) -> calendar:rfc3339_to_system_time(Time, [{unit, millisecond}]).

%%%%%%%%%%%%%%%%%
%%% Internal  %%%
%%%%%%%%%%%%%%%%%

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

in_cron_range(DateSpec, {{Year, Month, Day}, {Hour, Minute, Second}}) ->
    #{
        second := SecondSpec, minute := MinuteSpec, hour := HourSpec,
        day_of_month := DayOfMonthSpec, month := MonthSpec,
        day_of_week := DayOfWeekSpec
    } = DateSpec,
    Week = case calendar:day_of_the_week(Year, Month, Day) of 7 -> 0; DOW -> DOW end,
    in_cron_range2(Second, SecondSpec) andalso
        in_cron_range2(Minute, MinuteSpec) andalso
        in_cron_range2(Hour, HourSpec) andalso
        in_cron_range2(Month, MonthSpec) andalso
        in_cron_week_month_day_range(Week, Day, DayOfWeekSpec, DayOfMonthSpec).

in_cron_week_month_day_range(Week, Day, DayOfWeekSpec, DayOfMonthSpec) ->
    DOW = in_cron_range2(Week, DayOfWeekSpec),
    DOM = in_cron_range2(Day, DayOfMonthSpec),
    DayOfWeekSpec =/= '*' andalso DayOfMonthSpec =/= '*' andalso (DOW orelse DOM) orelse
        (DOW andalso DOM).

in_cron_range2(_, '*') -> true;
in_cron_range2(Value, ZipValues) -> lists:member(Value, prop_ecron:unzip(ZipValues)).

is_greater_than_or_equal(unlimited, _Datetime) -> true;
is_greater_than_or_equal(_, unlimited) -> true;
is_greater_than_or_equal(A, B) -> A >= B.
