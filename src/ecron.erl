-module(ecron).
-include("ecron.hrl").

-export([add/3, add/6]).
-export([add_with_datetime/4, add_with_datetime/5]).
-export([add_with_count/3, add_with_count/4]).
-export([send_interval/3, send_interval/5, send_interval/7]).
-export([send_after/3]).
-export([delete/1]).
-export([deactivate/1, activate/1]).
-export([statistic/0, statistic/1]).
-export([parse_spec/2, parse_spec/1]).
-export([valid_datetime/2]).

-type name() :: term().
-type crontab_spec() :: crontab() | string() | binary() | 1..4294967.

-type crontab() :: #{second => '*' | [0..59 | {0..58, 1..59}, ...],
minute => '*' | [0..59 | {0..58, 1..59}, ...],
hour => '*' | [0..23, ...],
month => '*' | [1..12 | {1..11, 2..12}, ...],
day_of_month => '*' | [1..31 | {1..30, 2..31}, ...],
day_of_week => '*' | [0..6 | {0..5, 1..6}, ...]}.

-type ecron() :: #{name => name(),
crontab => crontab(),
start_time => calendar:rfc3339_string() | unlimited,
end_time => calendar:rfc3339_string() | unlimited,
mfa => mfa(),
type => cron | every}.

-type status() :: waiting | running | deactivate | activate | already_ended.

-type statistic() :: #{ecron => ecron(),
status => status(),
failed => non_neg_integer(),
ok => non_neg_integer(),
results => [term()],
run_microsecond => [pos_integer()],
time_zone => local | utc,
worker => pid(),
next => [calendar:datetime()]}.

-type parse_error() :: invalid_time | invalid_spec | month | day_of_month | day_of_week | hour | minute | second.
-type start_datetime() :: unlimited | calendar:datetime().
-type end_datetime() :: unlimited | calendar:datetime().
-type option() :: {singleton, boolean()} |{max_count, pos_integer() | unlimited}.
-type options() :: [option()].

%% @equiv add(JobName, Spec, MFA, unlimited, unlimited, [])
-spec add(name(), crontab_spec(), mfa()) ->
    {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.
add(JobName, Spec, MFA) ->
    add(JobName, Spec, MFA, unlimited, unlimited, []).

%% @equiv add_with_count(make_ref(), Spec, MFA, RunCount).
-spec add_with_count(crontab_spec(), mfa(), pos_integer()) ->
    {ok, name()} | {error, parse_error(), term()}.
add_with_count(Spec, MFA, RunCount) when is_integer(RunCount) ->
    add_with_count(make_ref(), Spec, MFA, RunCount).

%% @equiv add(make_ref(), Spec, MFA, unlimited, unlimited, [{max_count, RunCount}])
-spec add_with_count(name(), crontab_spec(), mfa(), pos_integer()) ->
    {ok, name()} | {error, parse_error(), term()}.
add_with_count(JobName, Spec, MFA, RunCount) when is_integer(RunCount) ->
    add(JobName, Spec, MFA, unlimited, unlimited, [{max_count, RunCount}]).

%% @equiv add(make_ref(), Spec, MFA, Start, End, [])
-spec add_with_datetime(crontab_spec(), mfa(), start_datetime(), end_datetime()) ->
    {ok, name()} | {error, parse_error(), term()}.
add_with_datetime(Spec, MFA, Start, End) ->
    add(make_ref(), Spec, MFA, Start, End, []).

%% @equiv add(JobName, Spec, MFA, Start, End, [])
-spec add_with_datetime(name(), crontab_spec(), mfa(), start_datetime(), end_datetime()) ->
    {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.
add_with_datetime(JobName, Spec, MFA, Start, End) ->
    add(JobName, Spec, MFA, Start, End, []).

%% @doc
%% Add new crontab job. All jobs that exceed the limit will be automatically removed.
%% <ul>
%% <li>`JobName': The unique name of job, return `{error, already_exist}' if JobName is already exist.</li>
%% <li>`Spec': A <a href="https://github.com/zhongwencool/ecron/blob/master/README.md#cron-expression-format"><tt>cron expression</tt></a> represents a set of times.</li>
%% <li>`MFA': Spawn a process to run MFA when crontab is triggered.</li>
%% <li>`Start': The job's next trigger time is Calculated from StartDatetime. Keeping `unlimited' if start from now on.</li>
%% <li>`End': The job will be remove at end time. Keeping `unlimited' if never end.</li>
%% <li>`Opts': The optional list of options. `{singleton, true}': Default job is singleton, Each task cannot be executed concurrently.
%% `{max_count, pos_integer()}': This task can be run up to `MaxCount' times, default is `unlimited'.
%% </li>
%% </ul>
%%
-spec add(name(), crontab_spec(), mfa(), start_datetime(), end_datetime(), options()) ->
    {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.
add(JobName, Spec, MFA, Start, End, Option) ->
    case valid_datetime(Start, End) of
        true ->
            case parse_spec(Spec) of
                {ok, Type, Crontab} ->
                    ecron_tick:add(#{
                        type => Type, name => JobName,
                        crontab => Crontab, mfa => MFA,
                        start_time => Start, end_time => End
                    }, Option);
                ErrParse -> ErrParse
            end;
        false -> {error, invalid_time, {Start, End}}
    end.

%% @doc
%% Starts a timer which will send the message Msg to Dest when crontab is triggered.
%% <ul>
%% <li>Equivalent to `erlang:send_after/3' expect the `Time' format. </li>
%% <li>If Dest is a pid() it has to be a pid() of a local process, dead or alive.</li>
%% <li>The Time value can, in the current implementation, not be greater than <strong>4294967295</strong>. </li>
%% <li>If Dest is an atom(), it is supposed to be the name of a registered process. The process referred to by the name is looked up at the time of delivery. No error is given if the name does not refer to a process.</li>
%% <li>If Dest is a pid(), the timer will be automatically canceled if the process referred to by the pid() is not alive, or when the process exits.</li>
%% <li><strong>Warning:</strong> Cancels a timer by `erlang:cancel_timer(Ref)' not `ecron:delete/1'.</li>
%% </ul>
-spec send_after(crontab_spec(), pid()|atom(), term()) ->
    {ok, reference()} | {error, parse_error(), term()}.
send_after(Spec, Pid, Message) ->
    case parse_spec(Spec, 1) of
        {ok, #{next := [Next]}} ->
            NextMs = calendar:rfc3339_to_system_time(Next, [{'unit', second}]) * 1000,
            Time = NextMs - erlang:system_time(millisecond),
            {ok, erlang:send_after(Time, Pid, Message)};
        Err -> Err
    end.

%% @equiv send_interval(make_ref(), Spec, Pid, Message, unlimited, unlimited, [])
-spec send_interval(crontab_spec(), pid(), term()) ->
    {ok, name()} | {error, parse_error(), term()}.
send_interval(Spec, Pid, Message) ->
    send_interval(make_ref(), Spec, Pid, Message, unlimited, unlimited, []).

%% @equiv send_interval(make_ref(), Spec, self(), Message, Start, End, Option)
-spec send_interval(crontab_spec(), term(), start_datetime(), end_datetime(), options()) ->
    {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.
send_interval(Spec, Message, Start, End, Option) ->
    send_interval(make_ref(), Spec, self(), Message, Start, End, Option).

%% @doc
%% Evaluates Pid ! Message repeatedly when crontab is triggered.
%% (Pid can also be an atom of a registered name.)
%% <ul>
%% <li>`JobName': The unique name of job, return `{error, already_exist}' if the name is already exist.</li>
%% <li>`Spec': A <a href="https://github.com/zhongwencool/ecron/blob/master/README.md#cron-expression-format"><tt>cron expression</tt></a> represents a set of times.</li>
%% <li>`Pid': The target pid which receive message.</li>
%% <li>`Message': Any erlang term.</li>
%% <li>`Start': The job's next trigger time is Calculated from StartDatetime. Keeping `unlimited' if start from now on.</li>
%% <li>`End': The job will be remove at end time. Keeping `unlimited' if never end.</li>
%% <li>`Opts': The optional list of options. `{singleton, true}': Default job is singleton, Each task cannot be executed concurrently.
%% `{max_count, pos_integer()}': This task can be run up to `MaxCount' times, default is `unlimited'.
%% </li>
%% </ul>
%%
-spec send_interval(name(), crontab_spec(), pid(), term(), start_datetime(),
    end_datetime(), options()) ->
    {ok, name()} | {error, parse_error(), term()} | {error, already_exist}.
send_interval(JobName, Spec, Pid, Message, Start, End, Option) ->
    add(JobName, Spec, {erlang, send, [Pid, Message]}, Start, End, Option).

%% @doc
%% Delete an exist job, if the job is nonexistent, nothing happened.
-spec delete(name()) -> ok.
delete(JobName) -> ecron_tick:delete(JobName).

%% @doc
%% deactivate an exist job, if the job is nonexistent, return `{error, not_found}'.
%% just freeze the job, use @see activate/1 to unfreeze job.
-spec deactivate(name()) -> ok | {error, not_found}.
deactivate(JobName) -> ecron_tick:deactivate(JobName).

%% @doc
%% activate an exist job, if the job is nonexistent, return `{error, not_found}'.
%% if the job is already activate, nothing happened.
%% the same effect as reinstall the job from now on.
-spec activate(name()) -> ok | {error, not_found}.
activate(JobName) -> ecron_tick:activate(JobName).

%% @doc
%% Statistic from an exist job.
%% if the job is nonexistent, return `{error, not_found}'.
-spec statistic(name()) -> {ok, statistic()} | {error, not_found}.
statistic(JobName) -> ecron_tick:statistic(JobName).

%% @doc
%% Statistic for all jobs.
-spec statistic() -> [statistic()].
statistic() -> ecron_tick:statistic().

%% @doc
%% parse a crontab spec with next trigger time. For debug.
-spec parse_spec(crontab_spec(), pos_integer()) ->
    {ok, #{type => cron | every, crontab => crontab_spec(), next => [calendar:rfc3339_string()]}} |
    {error, atom(), term()}.
parse_spec({ok, Type, JobSpec}, Num) ->
    Job = #{type => Type, crontab => JobSpec},
    Next = ecron_tick:predict_datetime(Job, Num),
    {ok, Job#{next => Next}};
parse_spec({error, _Field, _Value} = Error, _Num) -> Error;
parse_spec(Spec, Num) when is_integer(Num) andalso Num > 0 ->
    parse_spec(parse_spec(Spec), Num).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
valid_datetime(Start, End) ->
    case valid_datetime(Start) andalso valid_datetime(End) of
        true when Start =/= unlimited andalso End =/= unlimited ->
            EndSec = calendar:datetime_to_gregorian_seconds(End),
            StartSec = calendar:datetime_to_gregorian_seconds(Start),
            EndSec > StartSec;
        Res -> Res
    end.

valid_datetime(unlimited) -> true;
valid_datetime({Date, {H, M, S}}) ->
    (is_integer(H) andalso H >= 0 andalso H =< 23) andalso
        (is_integer(M) andalso M >= 0 andalso M =< 59) andalso
        (is_integer(S) andalso S >= 0 andalso H =< 59) andalso
        calendar:valid_date(Date);
valid_datetime(_ErrFormat) -> false.

%% @private
parse_spec("@yearly") -> parse_spec("0 0 1 1 *");    % Run once a year, midnight, Jan. 1st
parse_spec("@annually") -> parse_spec("0 0 1 1 *");  % Same as @yearly
parse_spec("@monthly") -> parse_spec("0 0 1 * *");   % Run once a month, midnight, first of month
parse_spec("@weekly") -> parse_spec("0 0 * * 0");    % Run once a week, midnight between Sat/Sun
parse_spec("@midnight") -> parse_spec("0 0 * * *");  % Run once a day, midnight
parse_spec("@daily") -> parse_spec("0 0 * * *");     % Same as @midnight
parse_spec("@hourly") -> parse_spec("0 * * * *");    % Run once an hour, beginning of hour
parse_spec("@minutely") -> parse_spec("* * * * *");
parse_spec(Bin) when is_binary(Bin) -> parse_spec(binary_to_list(Bin));
parse_spec(List) when is_list(List) ->
    case string:tokens(string:lowercase(List), " ") of
        [_S, _M, _H, _DOM, _Mo, _DOW] = Cron -> parse_cron_spec(Cron);
        [_M, _H, _DOM, _Mo, _DOW] = Cron -> parse_cron_spec(["0" | Cron]);
        ["@every", Sec] -> parse_every_spec(Sec);
        _ -> {error, invalid_spec, List}
    end;
parse_spec(Spec) when is_map(Spec) ->
    {Months, NewSpec} = take(month, Spec),
    case unzip(Months, 1, 12, []) of
        {ok, EMonths} ->
            List = [{second, 0, 59}, {minute, 0, 59}, {hour, 0, 23},
                {day_of_month, 1, get_max_day_of_months(EMonths)},
                {day_of_week, 0, 6}],
            format_map_spec(List, NewSpec, #{month => zip(EMonths)});
        error -> {error, month, Months}
    end;
parse_spec(Second) when is_integer(Second) andalso Second =< ?MAX_TIMEOUT ->
    {ok, every, Second};
parse_spec(Spec) -> {error, invalid_spec, Spec}.

parse_cron_spec([Second, Minute, Hour, DayOfMonth, Month, DayOfWeek]) ->
    case parse_field(Month, 1, 12) of
        {ok, Months} ->
            Fields = [
                {second, Second, 0, 59},
                {minute, Minute, 0, 59},
                {hour, Hour, 0, 23},
                {day_of_month, DayOfMonth, 1, get_max_day_of_months(Months)},
                {day_of_week, DayOfWeek, 0, 6}],
            parse_fields(Fields, #{month => Months});
        error -> {error, month, Month}
    end.

parse_fields([], Acc) -> {ok, cron, Acc};
parse_fields([{Key, Spec, Min, Max} | Rest], Acc) ->
    case parse_field(Spec, Min, Max) of
        {ok, V} -> parse_fields(Rest, Acc#{Key => V});
        error -> {error, Key, Spec}
    end.

parse_field("*", _Min, _Max) -> {ok, '*'};
parse_field(Value, MinLimit, MaxLimit) ->
    parse_field(string:tokens(Value, ","), MinLimit, MaxLimit, []).

parse_field([], _MinLimit, _MaxLimit, Acc) -> {ok, zip(lists:usort(Acc))};
parse_field([Field | Fields], MinL, MaxL, Acc) ->
    case string:tokens(Field, "-") of
        [Field] ->
            case string:tokens(Field, "/") of
                [_] -> % Integer
                    Int = field_to_int(Field),
                    case Int >= MinL andalso Int =< MaxL of
                        true -> parse_field(Fields, MinL, MaxL, [Int | Acc]);
                        false -> error
                    end;
                ["*", StepStr] -> % */Step -> MinLimit~MaxLimit/Step
                    case field_to_int(StepStr) of
                        Step when Step > 0 ->
                            NewAcc = lists:seq(MinL, MaxL, Step) ++ Acc,
                            parse_field(Fields, MinL, MaxL, NewAcc);
                        _ -> error
                    end;
                [MinStr, StepStr] -> % Min/Step -> Min~MaxLimit/Step
                    Min = field_to_int(MinStr),
                    Step = field_to_int(StepStr),
                    case Min >= MinL andalso Min =< MaxL andalso Step > 0 of
                        true ->
                            NewAcc = lists:seq(Min, MaxL, Step) ++ Acc,
                            parse_field(Fields, MinL, MaxL, NewAcc);
                        false -> error
                    end;
                _ -> error
            end;
        [MinStr, MaxStepStr] ->
            case field_to_int(MinStr) of
                Min when Min >= MinL andalso Min =< MaxL -> % Min-Max/Step -> Min~Max/Step
                    {Max, Step} =
                        case string:tokens(MaxStepStr, "/") of
                            [_] -> {field_to_int(MaxStepStr), 1};
                            [MaxStr, StepStr] -> {field_to_int(MaxStr), field_to_int(StepStr)};
                            _ -> {-1, -1} %% error
                        end,
                    case Max >= MinL andalso Max >= Min andalso Step > 0 of
                        true ->
                            New = lists:seq(Min, Max, Step),
                            case lists:max(New) =< MaxL of
                                true -> parse_field(Fields, MinL, MaxL, New ++ Acc);
                                false -> error
                            end;
                        false -> error
                    end;
                _ -> error
            end;
        _ -> error
    end.

zip('*') -> '*';
zip([T | Rest]) -> zip(Rest, T + 1, [T], []).

zip([], _, [Single], Acc) -> lists:reverse([Single | Acc]);
zip([], _, [One, Two], Acc) -> lists:reverse([One, Two | Acc]);
zip([], _, Buffer, Acc) -> lists:reverse([{lists:min(Buffer), lists:max(Buffer)} | Acc]);
zip([L | Rest], L, Buffer, Acc) -> zip(Rest, L + 1, [L | Buffer], Acc);
zip([F | Rest], _Last, [Single], Acc) -> zip(Rest, F + 1, [F], [Single | Acc]);
zip([F | Rest], _Last, [One, Two], Acc) -> zip(Rest, F + 1, [F], [One, Two | Acc]);
zip([F | Rest], _Last, Buffer, Acc) ->
    zip(Rest, F + 1, [F], [{lists:min(Buffer), lists:max(Buffer)} | Acc]).

unzip('*', _MinLimit, _MaxLimit, _Acc) -> {ok, '*'};
unzip([], _MinLimit, _MaxLimit, Acc) -> {ok, lists:usort(Acc)};
unzip([{Min, Max} | List], MinL, MaxL, Acc) ->
    NewMin = field_to_int(Min),
    NewMax = field_to_int(Max),
    case NewMax >= NewMin andalso NewMax =< MaxL andalso NewMin >= MinL of
        true -> unzip(List, MinL, MaxL, lists:seq(NewMin, NewMax) ++ Acc);
        false -> error
    end;
unzip([Int | List], MinL, MaxL, Acc) ->
    case field_to_int(Int) of
        V when V >= MinL andalso V =< MaxL -> unzip(List, MinL, MaxL, [V | Acc]);
        _ -> error
    end.


parse_every_spec(SecSpec) ->
    LowerSecSpec = string:lowercase(SecSpec),
    List = [{"d", 24 * 3600}, {"h", 3600}, {"m", 60}, {"s", 1}],
    case parse_every(List, LowerSecSpec, 0) of
        {ok, Sec} when Sec > 0 andalso Sec =< ?MAX_TIMEOUT -> {ok, every, Sec};
        {ok, Sec} -> {error, second, Sec};
        error -> {error, second, SecSpec}
    end.

parse_every(_, "", Sum) -> {ok, Sum};
parse_every([], _, _Sum) -> error;
parse_every([{Sep, Index} | Rest], Spec, Sum) ->
    case parse_every(Spec, Sep) of
        {Val, NewSpec} -> parse_every(Rest, NewSpec, Val * Index + Sum);
        error -> error
    end.

parse_every(Spec, Seps) ->
    case string:tokens(Spec, Seps) of
        [Spec] -> {0, Spec};
        [Str, S] ->
            case field_to_int(Str) of
                Value when Value >= 0 -> {Value, S};
                _ -> error
            end;
        [Str] ->
            case field_to_int(Str) of
                Value when Value >= 0 -> {Value, ""};
                _ -> error
            end;
        _ -> error
    end.

get_max_day_of_months('*') -> 31;
get_max_day_of_months(List) -> max_day_of_months(List, 29).

max_day_of_months([], Max) -> Max;
max_day_of_months(_, 31) -> 31;
max_day_of_months([{_Min, _Max} | _List], _OldMax) -> 31; %% because Max - Min >= 2
max_day_of_months([Int | List], Max) ->
    NewMax = erlang:max(Max, last_day_of_month(Int)),
    max_day_of_months(List, NewMax).

last_day_of_month(2) -> 29;
last_day_of_month(4) -> 30;
last_day_of_month(6) -> 30;
last_day_of_month(9) -> 30;
last_day_of_month(11) -> 30;
last_day_of_month(M) when is_integer(M), M > 0, M < 13 -> 31.

-define(Alphabet, #{
    "sun" => 0, "mon" =>  1, "tue" =>  2, "wed" =>  3, "thu" =>  4, "fir" =>  5, "sat" =>  6,
    "jan" =>  1, "feb" =>  2, "mar" =>  3, "apr" =>  4, "may" =>  5, "jun" =>  6,
    "jul" =>  7, "aug" =>  8, "sep" =>  9, "oct" =>  10, "nov" =>  11, "dec" =>  12}).

field_to_int(Int) when is_integer(Int) -> Int;
field_to_int(List) when is_list(List) ->
    case maps:find(List, ?Alphabet) of
        error ->
            case string:list_to_integer(List) of
                {Int, []} -> Int;
                _ -> -1 %% error
            end;
        {ok, Int} -> Int
    end.

format_map_spec([], Old, New) when Old =:= #{} -> {ok, cron, New};
format_map_spec([], Old, _New) -> {error, maps:keys(Old), maps:values(Old)};
format_map_spec([{Key, Min, Max} | List], Old, New) ->
    {Value, Old1} = take(Key, Old),
    case unzip(Value, Min, Max, []) of
        {ok, EValue} -> format_map_spec(List, Old1, New#{Key => zip(EValue)});
        error -> {error, Key, Value}
    end.

take(Key, Spec) ->
    case maps:take(Key, Spec) of
        error when Key =:= second -> {[0], Spec};
        error -> {'*', Spec};
        Res -> Res
    end.

%% For PropEr Test
-ifdef(TEST).
-compile(export_all).
-endif.
