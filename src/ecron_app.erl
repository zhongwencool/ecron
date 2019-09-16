-module(ecron_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = ecron_sup:start_link(),
    case load_crontab() of
        ok -> {ok, Pid};
        Reason -> Reason
    end.

stop(_State) ->
    ok.

load_crontab() ->
    load_crontab(application:get_env(ecron, jobs, [])).
load_crontab([]) -> ok;
load_crontab([{Name, Spec, {_M, _F, _A} = MFA, Start, End} | Jobs]) ->
    case ecron:add(Name, Spec, MFA, Start, End) of
        {ok, _Pid} -> load_crontab(Jobs);
        {error, Field, Reason} -> {error, lists:flatten(io_lib:format("~p: ~p", [Field, Reason]))}
    end;
load_crontab([{Name, Spec, {_M, _F, _A} = MFA} | Jobs]) ->
    case ecron:add(Name, Spec, MFA) of
        {ok, _Pid} -> load_crontab(Jobs);
        {error, Field, Reason} -> {error, lists:flatten(io_lib:format("~p: ~p", [Field, Reason]))}
    end;
load_crontab([L|_]) -> {error, L}.
