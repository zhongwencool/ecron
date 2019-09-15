-module(ecron_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = ecron_sup:start_link(),
    case load_crontab_from_config() of
        ok -> {ok, Pid};
        Reason -> Reason
    end.

stop(_State) ->
    ok.

load_crontab_from_config() ->
    load_crontab_from_config(application:get_env(ecron, jobs, [])).
load_crontab_from_config([]) -> ok;
load_crontab_from_config([{Name, Spec, {_M, _F, _A} = MFA, Start, End} | Jobs]) ->
    case ecron:add(Name, Spec, MFA, Start, End) of
        {ok, _Pid} -> load_crontab_from_config(Jobs);
        Error -> Error
    end;
load_crontab_from_config([{Name, Spec, {_M, _F, _A} = MFA} | Jobs]) ->
    case ecron:add(Name, Spec, MFA) of
        {ok, _Pid} -> load_crontab_from_config(Jobs);
        Error -> Error
    end.
