-module(prop_ecron_parse).
-include_lib("proper/include/proper.hrl").

-export([prop_zip/0, prop_zip/1]).
-export([prop_get_max_day_of_months/0, prop_get_max_day_of_months/1]).
-export([prop_valid_datetime/0, prop_valid_datetime/1]).
-export([prop_parse_spec_extend/0, prop_parse_spec_extend/1]).
-export([prop_parse_spec_without_second/0, prop_parse_spec_without_second/1]).
-export([prop_parse_predefined_spec/0, prop_parse_predefined_spec/1]).
-export([prop_parse_every_spec/0, prop_parse_every_spec/1]).
-export([prop_parse_error_spec/0, prop_parse_error_spec/1]).
-export([prop_parse_map_error_spec/0, prop_parse_map_error_spec/1]).

-import(prop_ecron_helper, [spec/0, maybe_error_spec/0, extend_spec/0,spec_to_str/1, unzip/1]).
-import(prop_ecron_helper, [check_day_of_month/1, field_to_extend/1, cron_spec_to_map/1, get_max_day_of_months/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_zip(doc) -> "ecron:zip/1 failed";
prop_zip(opts) -> [{numtests, 4000}].
prop_zip() ->
    ?FORALL(List, usort_list(non_neg_integer()),
        unzip(ecron:zip(List)) =:= List).

prop_get_max_day_of_months(doc) -> "ecron:get_max_day_of_months/1 failed";
prop_get_max_day_of_months(opts) -> [{numtests, 4000}].
prop_get_max_day_of_months() ->
    ?FORALL(List, usort_list(range(1, 12)),
        begin
            ZipList = ecron:zip(List),
            Actual = ecron:get_max_day_of_months(ZipList),
            Expect = get_max_day_of_months(List),
            Expect =:= Actual
        end).

prop_valid_datetime(doc) -> "ecron:valid_datetime/2 failed";
prop_valid_datetime(opts) -> [{numtests, 4000}].
prop_valid_datetime() ->
    ?FORALL({Date, Time}, {{year(), month(), day()}, {hour(), minute(), second()}},
        ?IMPLIES(calendar:valid_date(Date), ecron:valid_datetime({Date, Time}))).

prop_parse_spec_extend(doc) -> "ecron:parse_spec(\"second minute hour day_of_month month day_of_week\") failed";
prop_parse_spec_extend(opts) -> [{numtests, 4000}].
prop_parse_spec_extend() ->
    ?FORALL(Spec, extend_spec(),
        ?IMPLIES(check_day_of_month(Spec),
            begin
                SpecStr = spec_to_str(Spec),
                Actual = ecron:parse_spec(SpecStr),
                Expect = spec_to_extend(Spec),
                ?WHENFAIL(
                    io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, Actual]),
                    Expect =:= Actual
                )
            end)
    ).

prop_parse_spec_without_second(doc) -> "ecron:parse_spec(\"minute hour day_of_month month day_of_week\") failed";
prop_parse_spec_without_second(opts) -> [{numtests, 4000}].
prop_parse_spec_without_second() ->
    ?FORALL(Spec, spec(),
        ?IMPLIES(check_day_of_month(Spec),
            begin
                SpecStr = spec_to_str(Spec),
                Actual = ecron:parse_spec(SpecStr),
                Expect = spec_to_extend(Spec),
                ?WHENFAIL(
                    io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, Actual]),
                    Expect =:= Actual
                )
            end)
    ).

-define(PredefinedSpec, ["@yearly", "@annually", "@monthly", "@weekly", "@midnight", "@daily", "@hourly"]).

prop_parse_predefined_spec(doc) -> "ecron:parse_spec/1 predefined spec failed";
prop_parse_predefined_spec(opts) -> [{numtests, 2000}].
prop_parse_predefined_spec() ->
    ?FORALL(Spec, elements(?PredefinedSpec),
        begin
            Actual = ecron:parse_spec(Spec),
            Expect = spec_to_extend(Spec),
            ?WHENFAIL(
                io:format("Parse ~s SpecFailed: ~p\n  ~p\n", [Spec, Expect, Actual]),
                Expect =:= Actual
            )
        end).

-define(MAX_TIMEOUT, 4294967). %% (16#ffffffff div 1000) 49.71 days.

prop_parse_every_spec(doc) -> "ecron:parse_spec/1 @every 1d2h3m4s spec failed";
prop_parse_every_spec(opts) -> [{numtests, 4000}].
prop_parse_every_spec() ->
    ?FORALL(Spec, every_spec(),
        ?IMPLIES(every_spec_to_second(Spec) =< ?MAX_TIMEOUT,
            begin
                {Format, Args} = every_spec_to_str(Spec),
                SpecStr = lists:flatten(io_lib:format("@every " ++ Format, Args)),
                Actual = ecron:parse_spec(SpecStr),
                Expect = {ok, every, every_spec_to_second(Spec) * 1000},
                ?WHENFAIL(
                    io:format("Parse ~s Failed: ~p\n~p\n", [SpecStr, Expect, Actual]),
                    Expect =:= Actual
                )
            end)
    ).

prop_parse_error_spec(doc) -> "ecron:parse_spec/1 error spec failed";
prop_parse_error_spec(opts) -> [{numtests, 4000}].
prop_parse_error_spec() ->
    ?FORALL(Spec, maybe_error_spec(),
        begin
            SpecStr = spec_to_str(Spec),
            {ok, cron, Expect} = spec_to_extend(Spec),
            case ecron:parse_spec(SpecStr) of
                {ok, cron, Ecron} ->
                    ?WHENFAIL(
                        io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, Ecron]),
                        Expect =:= Ecron
                    );
                {error, Field, Reason} ->
                    ?WHENFAIL(
                        io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, {Field, Reason}]),
                        check_failed_field(Field, maps:get(Field, Expect))
                    )
            end
        end).

prop_parse_map_error_spec(doc) -> "parse error spec failed";
prop_parse_map_error_spec(opts) -> [{numtests, 4000}].
prop_parse_map_error_spec() ->
    ?FORALL(Spec, maybe_error_spec(),
        begin
            SpecStr = cron_spec_to_map(Spec),
            {ok, cron, Expect} = spec_to_extend(Spec),
            case ecron:parse_spec(SpecStr) of
                {ok, cron, Ecron} ->
                    ?WHENFAIL(
                        io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, Ecron]),
                        Expect =:= Ecron
                    );
                {error, Field, Reason} ->
                    ?WHENFAIL(
                        io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, {Field, Reason}]),
                        check_failed_field(Field, maps:get(Field, Expect))
                    )
            end
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

-define(DAY, 24 * ?HOUR).
-define(HOUR, 60 * ?MINUTE).
-define(MINUTE, 60).

every_spec_to_second({day, Day}) -> Day * ?DAY;
every_spec_to_second({hour, Hour}) -> Hour * ?HOUR;
every_spec_to_second({minute, Minute}) -> Minute * ?MINUTE;
every_spec_to_second({second, Second}) -> Second;
every_spec_to_second({day_hour, Day, Hour}) -> Day * ?DAY + Hour * ?HOUR;
every_spec_to_second({day_minute, Day, Minute}) -> Day * ?DAY + Minute * ?MINUTE;
every_spec_to_second({day_second, Day, Second}) -> Day * ?DAY + Second;
every_spec_to_second({hour_minute, Hour, Minute}) -> Hour * ?HOUR + Minute * ?MINUTE;
every_spec_to_second({hour_second, Hour, Second}) -> Hour * ?HOUR + Second;
every_spec_to_second({minute_second, Minute, Second}) -> Minute * ?MINUTE + Second;
every_spec_to_second({day_hour_minute, Day, Hour, Minute}) -> Day * ?DAY + Hour * ?HOUR + Minute * ?MINUTE;
every_spec_to_second({day_hour_second, Day, Hour, Second}) -> Day * ?DAY + Hour * ?HOUR + Second;
every_spec_to_second({hour_minute_second, Hour, Minute, Second}) -> Hour * ?HOUR + Minute * ?MINUTE + Second;
every_spec_to_second({day_hour_minute_sec, Day, Hour, Minute, Second}) ->
    Day * ?DAY + Hour * ?HOUR + Minute * ?MINUTE + Second.

every_spec_to_str({day, Day}) -> {"~wd", [Day]};
every_spec_to_str({hour, Hour}) -> {"~wh", [Hour]};
every_spec_to_str({minute, Minute}) -> {"~wm", [Minute]};
every_spec_to_str({second, Second}) -> {"~ws", [Second]};
every_spec_to_str({day_hour, Day, Hour}) -> {"~wd~wh", [Day, Hour]};
every_spec_to_str({day_minute, Day, Minute}) -> {"~wd~wm", [Day, Minute]};
every_spec_to_str({day_second, Day, Second}) -> {"~wd~ws", [Day, Second]};
every_spec_to_str({hour_minute, Hour, Minute}) -> {"~wh~wm", [Hour, Minute]};
every_spec_to_str({hour_second, Hour, Second}) -> {"~wh~ws", [Hour, Second]};
every_spec_to_str({minute_second, Minute, Second}) -> {"~wm~ws", [Minute, Second]};
every_spec_to_str({day_hour_minute, Day, Hour, Minute}) -> {"~wd~wh~wm", [Day, Hour, Minute]};
every_spec_to_str({day_hour_second, Day, Hour, Second}) -> {"~wd~wh~ws", [Day, Hour, Second]};
every_spec_to_str({hour_minute_second, Hour, Minute, Second}) -> {"~wh~wm~ws", [Hour, Minute, Second]};
every_spec_to_str({day_hour_minute_sec, Day, Hour, Minute, Second}) -> {"~wd~wh~wm~ws", [Day, Hour, Minute, Second]}.

%% minute hour day_of_month month day_of_week
spec_to_extend("@yearly") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, {'Integer', general, 1}, {'Integer', general, 1}, "*"]);
spec_to_extend("@annually") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, {'Integer', general, 1}, {'Integer', general, 1}, "*"]);
spec_to_extend("@monthly") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, {'Integer', general, 1}, "*", "*"]);
spec_to_extend("@weekly") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, "*", "*", {'Integer', general, 0}]);
spec_to_extend("@midnight") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, "*", "*", "*"]);
spec_to_extend("@daily") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, "*", "*", "*"]);
spec_to_extend("@hourly") ->
    spec_to_extend([{'Integer', general, 0}, "*", "*", "*", "*"]);
spec_to_extend([Minute, Hour, Day, Month, Week]) ->
    spec_to_extend([{'Integer', general, 0}, Minute, Hour, Day, Month, Week]);
spec_to_extend([Second, Minute, Hour, Day, Month, Week]) ->
    {ok, cron, #{
        second => field_to_extend(Second),
        minute => field_to_extend(Minute),
        hour => field_to_extend(Hour),
        day_of_month => field_to_extend(Day),
        month =>field_to_extend(Month),
        day_of_week => field_to_extend(Week)
    }}.

check_failed_field(second, List) -> not lists:all(fun(L) -> L >= 0 andalso L =< 59 end, List);
check_failed_field(minute, List) -> not lists:all(fun(L) -> L >= 0 andalso L =< 59 end, List);
check_failed_field(hour, List) -> not lists:all(fun(L) -> L >= 0 andalso L =< 23 end, List);
check_failed_field(day_of_month, List) -> not lists:all(fun(L) -> L >= 1 andalso L =< 29 end, List);
check_failed_field(month, List) -> not lists:all(fun(L) -> L >= 1 andalso L =< 12 end, List);
check_failed_field(day_of_week, List) -> not lists:all(fun(L) -> L >= 0 andalso L =< 6 end, List).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

usort_list(T) ->
    non_empty(?LET(L, list(T), lists:usort(L))).

year() -> range(0, 4000).
month() -> range(1, 12).
day() -> range(1, 31).
hour() -> range(0, 23).
minute() -> range(0, 59).
second() -> range(0, 59).

every_spec() ->
    oneof([
        {day, pos_integer()},
        {hour, pos_integer()},
        {minute, pos_integer()},
        {second, pos_integer()},
        {day_hour, pos_integer(), pos_integer()},
        {day_minute, pos_integer(), pos_integer()},
        {day_second, pos_integer(), pos_integer()},
        {hour_minute, pos_integer(), pos_integer()},
        {hour_second, pos_integer(), pos_integer()},
        {minute_second, pos_integer(), pos_integer()},
        {day_hour_minute, pos_integer(), pos_integer(), pos_integer()},
        {day_hour_second, pos_integer(), pos_integer(), pos_integer()},
        {hour_minute_second, pos_integer(), pos_integer(), pos_integer()},
        {day_hour_minute_sec, pos_integer(), pos_integer(), pos_integer(), pos_integer()}
    ]).
