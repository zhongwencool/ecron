-module(ecron_monitor).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Jobs) ->
    gen_server:start_link(?MODULE, [Jobs], []).

init([Jobs]) ->
    case ecron_tick:parse_crontab(Jobs, []) of
        {ok, [_ | _]} ->
            erlang:process_flag(trap_exit, true),
            ok = net_kernel:monitor_nodes(true),
            {ok, undefined, 5};
        {stop, Reason} -> {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Reason, State) ->
    QuorumSize = application:get_env(ecron, cluster_quorum_size, 1),
    case erlang:length(nodes([this, visible])) >= QuorumSize of
        true ->
            {ok, Pid} = ecron_sup:start_global(Reason),
            link(Pid);
        false -> ecron_sup:stop_global(Reason)
    end,
    {noreply, State}.
