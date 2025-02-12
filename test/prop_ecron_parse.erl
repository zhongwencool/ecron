-module(prop_ecron_parse).
-include_lib("proper/include/proper.hrl").
-include_lib("ecron/include/ecron.hrl").

-export([prop_zip/0, prop_zip/1]).
-export([prop_max_day_of_months/0, prop_max_day_of_months/1]).
-export([prop_spec_extend/0, prop_spec_extend/1]).
-export([prop_spec_standard/0, prop_spec_standard/1]).
-export([prop_spec_predefined/0, prop_spec_predefined/1]).
-export([prop_spec_every/0, prop_spec_every/1]).
-export([prop_spec_every_error/0, prop_spec_every_error/1]).
-export([prop_maybe_error_spec/0, prop_maybe_error_spec/1]).
-export([prop_map_error_spec/0, prop_map_error_spec/1]).
-export([prop_error_format/0, prop_error_format/1]).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_zip(doc) -> "ecron:zip/1 failed";
prop_zip(opts) -> [{numtests, 4000}].
prop_zip() ->
    ?FORALL(
        List,
        usort_list(non_neg_integer()),
        prop_ecron:unzip(ecron_spec:zip(List)) =:= List
    ).

prop_max_day_of_months(doc) -> "ecron:get_max_day_of_months/1 failed";
prop_max_day_of_months(opts) -> [{numtests, 1000}].
prop_max_day_of_months() ->
    ?FORALL(
        List,
        usort_list(prop_ecron_spec:month()),
        begin
            ZipList = ecron_spec:zip(List),
            Actual = ecron_spec:get_max_day_of_months(ZipList),
            Expect = lists:max(lists:map(fun prop_ecron:max_day_of_month/1, List)),
            Expect =:= Actual
        end
    ).

prop_spec_extend(doc) ->
    "ecron_spec:parse_spec(\"second minute hour day_of_month month day_of_week\") failed";
prop_spec_extend(opts) ->
    [{numtests, 4000}].
prop_spec_extend() ->
    ?FORALL(
        Spec,
        prop_ecron_spec:extend_spec(),
        ?IMPLIES(
            prop_ecron:check_day_of_month(Spec),
            begin
                SpecStr = prop_ecron:spec_to_str(Spec),
                {ok, #{type := cron, crontab := Actual, next := Next}} = ecron:parse_spec(
                    SpecStr, 10
                ),
                {ok, cron, Expect} = spec_to_extend(Spec),
                ?WHENFAIL(
                    io:format("Parse ~s \nFailed: ~p\n  ~p\n", [SpecStr, Expect, Actual]),
                    Expect =:= Actual andalso length(Next) =:= 10
                )
            end
        )
    ).

prop_spec_standard(doc) ->
    "ecron_spec:parse_spec(\"minute hour day_of_month month day_of_week\") failed";
prop_spec_standard(opts) ->
    [{numtests, 4000}].
prop_spec_standard() ->
    ?FORALL(
        Spec,
        prop_ecron_spec:standard_spec(),
        ?IMPLIES(
            prop_ecron:check_day_of_month(Spec),
            begin
                SpecStr = prop_ecron:spec_to_str(Spec),
                Actual = ecron_spec:parse_spec(SpecStr),
                Expect = spec_to_extend(Spec),
                ?WHENFAIL(
                    io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, Actual]),
                    Expect =:= Actual
                )
            end
        )
    ).

-define(PredefinedSpec, [
    "@yearly", "@annually", "@monthly", "@weekly", "@midnight", "@daily", "@hourly", "@minutely"
]).

prop_spec_predefined(doc) -> "ecron_spec:parse_spec/1 predefined spec failed";
prop_spec_predefined(opts) -> [{numtests, 2000}].
prop_spec_predefined() ->
    ?FORALL(
        Spec,
        elements(?PredefinedSpec),
        begin
            Actual = ecron_spec:parse_spec(Spec),
            Expect = spec_to_extend(Spec),
            ?WHENFAIL(
                io:format("Parse ~s SpecFailed: ~p\n  ~p\n", [Spec, Expect, Actual]),
                Expect =:= Actual
            )
        end
    ).

prop_spec_every(doc) -> "ecron_spec:parse_spec/1 @every 1d2h3m4s spec failed";
prop_spec_every(opts) -> [{numtests, 4000}].
prop_spec_every() ->
    ?FORALL(
        Spec,
        every_spec(),
        ?IMPLIES(
            every_spec_to_second(Spec) =< ?MAX_TIMEOUT,
            begin
                {Format, Args} = every_spec_to_str(Spec),
                SpecStr = lists:flatten(io_lib:format("@every " ++ Format, Args)),
                Actual = ecron_spec:parse_spec(SpecStr),
                Expect = {ok, every, every_spec_to_second(Spec)},
                ?WHENFAIL(
                    io:format("Parse ~s Failed: ~p\n~p\n", [SpecStr, Expect, Actual]),
                    Expect =:= Actual
                )
            end
        )
    ).

prop_spec_every_error(doc) -> "ecron_spec:parse_spec/1 @every 1d2h3m4s spec failed";
prop_spec_every_error(opts) -> [{numtests, 4000}].
prop_spec_every_error() ->
    ?FORALL(
        Spec,
        every_error_spec(),
        begin
            {Format, Args} = every_spec_to_str(Spec),
            SpecStr = lists:flatten(io_lib:format("@every " ++ Format, Args)),
            {error, Second, Reason} = ecron:parse_spec(SpecStr, 10),
            ?WHENFAIL(
                io:format("Parse ~s Failed: ~p\n~p\n", [SpecStr, Second, Reason]),
                lists:member(Second, [second, invalid_spec])
            )
        end
    ).

prop_maybe_error_spec(doc) -> "ecron_spec:parse_spec/1 error spec failed";
prop_maybe_error_spec(opts) -> [{numtests, 4000}].
prop_maybe_error_spec() ->
    ?FORALL(
        Spec,
        prop_ecron_spec:maybe_error_spec(),
        begin
            SpecStr = prop_ecron:spec_to_str(Spec),
            {ok, cron, Expect} = spec_to_extend(Spec),
            case ecron:parse_spec(SpecStr, 10) of
                {ok, #{type := cron, crontab := Ecron}} ->
                    ?WHENFAIL(
                        io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, Ecron]),
                        Expect =:= Ecron
                    );
                {error, Field, Reason} ->
                    ?WHENFAIL(
                        io:format("Parse ~s Failed: ~p\n  ~p\n", [SpecStr, Expect, {Field, Reason}]),
                        check_failed_field(Field, prop_ecron:unzip(maps:get(Field, Expect)))
                    )
            end
        end
    ).

-define(Fields, [
    invalid_time, invalid_spec, month, day_of_month, day_of_week, hour, minute, second, no_change
]).

prop_map_error_spec(doc) -> "parse error map spec failed";
prop_map_error_spec(opts) -> [{numtests, 5000}].
prop_map_error_spec() ->
    ?FORALL(
        {Spec, Extra},
        {
            prop_ecron_spec:maybe_error_spec(),
            frequency([{1, #{wrong => [12]}}, {10, elements(?Fields)}])
        },
        begin
            SpecMap =
                case is_atom(Extra) of
                    true -> maps:remove(Extra, prop_ecron:cron_spec_to_map(Spec));
                    false -> Extra
                end,
            {ok, cron, Expect} = spec_to_extend(Spec),
            NewExpect =
                case Extra of
                    no_change -> Expect;
                    invalid_time -> Expect;
                    invalid_spec -> Expect;
                    second -> maps:put(Extra, [0], Expect);
                    _ -> maps:put(Extra, '*', Expect)
                end,
            case ecron_spec:parse_spec(SpecMap) of
                {ok, cron, Ecron} ->
                    ?WHENFAIL(
                        io:format("Parse ~p\n Failed: ~p\n  ~p\n", [SpecMap, NewExpect, Ecron]),
                        NewExpect =:= Ecron
                    );
                {error, Field, Reason} ->
                    ?WHENFAIL(
                        io:format("Parse ~p\n Failed: ~p\n  ~p\n", [
                            SpecMap, NewExpect, {Field, Reason}
                        ]),
                        check_failed_field(Field, prop_ecron:unzip(maps:get(Field, NewExpect, [])))
                    )
            end
        end
    ).

prop_error_format(doc) -> "ecron_spec:parse_spec/1 spec wrong format failed";
prop_error_format(opts) -> [{numtests, 2000}].
prop_error_format() ->
    ?FORALL(
        {Spec, Rand, Extra},
        {prop_ecron_spec:extend_spec(), range(1, 6), oneof([" spec ", "s", "/1/2", "x-1"])},
        begin
            SpecStr = spec_to_str(Spec, Rand, Extra),
            {error, Field, Reason} = ecron:add(error_spec, SpecStr, {erlang, datetime, []}),
            ?WHENFAIL(
                io:format("Parse ~s Failed: \n  ~p\n", [SpecStr, {Field, Reason}]),
                lists:member(Field, ?Fields)
            )
        end
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

-define(DAY, 24 * ?HOUR).
-define(HOUR, 60 * ?MINUTE).
-define(MINUTE, 60).

every_spec_to_second({day, Day}) ->
    Day * ?DAY;
every_spec_to_second({hour, Hour}) ->
    Hour * ?HOUR;
every_spec_to_second({minute, Minute}) ->
    Minute * ?MINUTE;
every_spec_to_second({second, Second}) ->
    Second;
every_spec_to_second({day_hour, Day, Hour}) ->
    Day * ?DAY + Hour * ?HOUR;
every_spec_to_second({day_minute, Day, Minute}) ->
    Day * ?DAY + Minute * ?MINUTE;
every_spec_to_second({day_second, Day, Second}) ->
    Day * ?DAY + Second;
every_spec_to_second({hour_minute, Hour, Minute}) ->
    Hour * ?HOUR + Minute * ?MINUTE;
every_spec_to_second({hour_second, Hour, Second}) ->
    Hour * ?HOUR + Second;
every_spec_to_second({minute_second, Minute, Second}) ->
    Minute * ?MINUTE + Second;
every_spec_to_second({day_hour_minute, Day, Hour, Minute}) ->
    Day * ?DAY + Hour * ?HOUR + Minute * ?MINUTE;
every_spec_to_second({day_hour_second, Day, Hour, Second}) ->
    Day * ?DAY + Hour * ?HOUR + Second;
every_spec_to_second({hour_minute_second, Hour, Minute, Second}) ->
    Hour * ?HOUR + Minute * ?MINUTE + Second;
every_spec_to_second({day_hour_minute_sec, Day, Hour, Minute, Second}) ->
    Day * ?DAY + Hour * ?HOUR + Minute * ?MINUTE + Second.

every_spec_to_str({format_error, 1, Str}) ->
    {"~s1d2m3s", [Str]};
every_spec_to_str({format_error, 2, Str}) ->
    {"1d~s2m3s", [Str]};
every_spec_to_str({format_error, 3, Str}) ->
    {"1d2m~s3s", [Str]};
every_spec_to_str({format_error, 4, Str}) ->
    {"1d2m3s~s", [Str]};
every_spec_to_str({too_big, Day}) ->
    {"~wd", [Day]};
every_spec_to_str({day, Day}) ->
    {"~wd", [Day]};
every_spec_to_str({hour, Hour}) ->
    {"~wh", [Hour]};
every_spec_to_str({minute, Minute}) ->
    {"~wm", [Minute]};
every_spec_to_str({second, Second}) ->
    {"~ws", [Second]};
every_spec_to_str({day_hour, Day, Hour}) ->
    {"~wd~wh", [Day, Hour]};
every_spec_to_str({day_minute, Day, Minute}) ->
    {"~wd~wm", [Day, Minute]};
every_spec_to_str({day_second, Day, Second}) ->
    {"~wd~ws", [Day, Second]};
every_spec_to_str({hour_minute, Hour, Minute}) ->
    {"~wh~wm", [Hour, Minute]};
every_spec_to_str({hour_second, Hour, Second}) ->
    {"~wh~ws", [Hour, Second]};
every_spec_to_str({minute_second, Minute, Second}) ->
    {"~wm~ws", [Minute, Second]};
every_spec_to_str({day_hour_minute, Day, Hour, Minute}) ->
    {"~wd~wh~wm", [Day, Hour, Minute]};
every_spec_to_str({day_hour_second, Day, Hour, Second}) ->
    {"~wd~wh~ws", [Day, Hour, Second]};
every_spec_to_str({hour_minute_second, Hour, Minute, Second}) ->
    {"~wh~wm~ws", [Hour, Minute, Second]};
every_spec_to_str({day_hour_minute_sec, Day, Hour, Minute, Second}) ->
    {"~wd~wh~wm~ws", [Day, Hour, Minute, Second]}.

%% minute hour day_of_month month day_of_week
spec_to_extend("@yearly") ->
    spec_to_extend([
        {'Integer', general, 0},
        {'Integer', general, 0},
        {'Integer', general, 1},
        {'Integer', general, 1},
        "*"
    ]);
spec_to_extend("@annually") ->
    spec_to_extend([
        {'Integer', general, 0},
        {'Integer', general, 0},
        {'Integer', general, 1},
        {'Integer', general, 1},
        "*"
    ]);
spec_to_extend("@monthly") ->
    spec_to_extend([
        {'Integer', general, 0}, {'Integer', general, 0}, {'Integer', general, 1}, "*", "*"
    ]);
spec_to_extend("@weekly") ->
    spec_to_extend([
        {'Integer', general, 0}, {'Integer', general, 0}, "*", "*", {'Integer', general, 0}
    ]);
spec_to_extend("@midnight") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, "*", "*", "*"]);
spec_to_extend("@daily") ->
    spec_to_extend([{'Integer', general, 0}, {'Integer', general, 0}, "*", "*", "*"]);
spec_to_extend("@hourly") ->
    spec_to_extend([{'Integer', general, 0}, "*", "*", "*", "*"]);
spec_to_extend("@minutely") ->
    spec_to_extend(["*", "*", "*", "*", "*"]);
spec_to_extend([Minute, Hour, Day, Month, Week]) ->
    spec_to_extend([{'Integer', general, 0}, Minute, Hour, Day, Month, Week]);
spec_to_extend([Second, Minute, Hour, Day, Month, Week]) ->
    {ok, cron, #{
        second => prop_ecron:field_to_extend(Second),
        minute => prop_ecron:field_to_extend(Minute),
        hour => prop_ecron:field_to_extend(Hour),
        day_of_month => prop_ecron:field_to_extend(Day),
        month => prop_ecron:field_to_extend(Month),
        day_of_week => prop_ecron:field_to_extend(Week)
    }}.

check_failed_field(second, List) ->
    not lists:all(fun(L) -> L >= 0 andalso L =< 59 end, List);
check_failed_field(minute, List) ->
    not lists:all(fun(L) -> L >= 0 andalso L =< 59 end, List);
check_failed_field(hour, List) ->
    not lists:all(fun(L) -> L >= 0 andalso L =< 23 end, List);
check_failed_field(day_of_month, List) ->
    not lists:all(fun(L) -> L >= 1 andalso L =< 29 end, List);
check_failed_field(month, List) ->
    not lists:all(fun(L) -> L >= 1 andalso L =< 12 end, List);
check_failed_field(day_of_week, List) ->
    not lists:all(fun(L) -> L >= 0 andalso L =< 6 end, List);
check_failed_field([wrong], _) ->
    true.

spec_to_str(Spec, Rand, Extra) ->
    Format = lists:duplicate(length(Spec), "~s"),
    {Format1, Format2} = lists:split(Rand, Format),
    NewFormat = string:join(Format1, " ") ++ "~s" ++ string:join(Format2, " "),
    SpecTmp = [prop_ecron:field_spec_to_str(S) || S <- Spec],
    {List1, List2} = lists:split(Rand, SpecTmp),
    SpecTmp2 = List1 ++ [Extra] ++ List2,
    iolist_to_binary(io_lib:format(NewFormat, SpecTmp2)).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

usort_list(T) ->
    non_empty(?LET(L, list(T), lists:usort(L))).

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

every_error_spec() ->
    oneof([
        {format_error, range(1, 4), elements(["a", "b", "c,", "d,"])},
        {too_big, range(?MAX_TIMEOUT + 1, ?MAX_TIMEOUT + 1000)}
    ]).
