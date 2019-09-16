-module(prop_ecron_helper).

-include_lib("proper/include/proper.hrl").
-export([spec_to_str/1, cron_spec_to_map/1]).
-export([unzip/1]).
-export([check_day_of_month/1]).
-export([field_to_extend/1]).
-export([to_now_datetime/2]).
-export([field_spec_to_str/1]).
-export([max_day_of_month/1]).

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

to_now_datetime(_, unlimited) -> unlimited;
to_now_datetime(utc, Time) -> calendar:system_time_to_universal_time(calendar:rfc3339_to_system_time(Time), second);
to_now_datetime(local, Time) -> calendar:system_time_to_local_time(calendar:rfc3339_to_system_time(Time), second).

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
