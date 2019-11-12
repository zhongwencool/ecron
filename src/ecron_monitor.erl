%%% @private
-module(ecron_monitor).
-behaviour(gen_server).

-export([start_link/2]).
-export([health/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Name, Jobs) ->
    gen_server:start_link(Name, ?MODULE, [Jobs], []).

init([Jobs]) ->
    case ecron_tick:parse_crontab(Jobs, []) of
        {ok, [_ | _]} ->
            erlang:process_flag(trap_exit, true),
            ok = net_kernel:monitor_nodes(true, [nodedown_reason]),
            {ok, undefined, 25};
        {stop, Reason} -> {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    QuorumSize = application:get_env(ecron, global_quorum_size, 1),
    {ResL, Bad} = rpc:multicall([node() | nodes(visible)], ?MODULE, health, [], 5000),
    {Healthy, GoodNodes, BadNodes} = split(ResL, 0, [], Bad),
    Measurements = #{action_ms => erlang:system_time(millisecond),
        quorum_size => QuorumSize, good_nodes => GoodNodes, bad_nodes => BadNodes},
    case Healthy >= QuorumSize of
        true ->
            {ok, Pid} = ecron_sup:start_global(Measurements),
            link(Pid);
        false -> ecron_sup:stop_global(Measurements)
    end,
    {noreply, State}.

health() ->
    case erlang:whereis(ecron) of
        undefined -> {error, node()};
        _ -> {ok, node()}
    end.

split([], Len, Good, Bad) -> {Len, Good, Bad};
split([{ok, Node} | Res], Len, Good, Bad) -> split(Res, Len + 1, [Node | Good], Bad);
split([{error, Node} | Res], Len, Good, Bad) -> split(Res, Len, Good, [Node | Bad]).
