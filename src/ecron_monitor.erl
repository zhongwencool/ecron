-module(ecron_monitor).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    erlang:process_flag(trap_exit, true),
    ok = net_kernel:monitor_nodes(true),
    {ok, undefined, 5}.

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    QuorumSize = application:get_env(ecron, cluster_quorum_size, 1),
    io:format("~p ~p ~n", [Info, QuorumSize]),
    case erlang:length(nodes([this, visible])) >= QuorumSize of
        true ->
            {ok, Pid} = ecron_sup:start_global(),
            link(Pid);
        false -> ecron_sup:stop_global()
    end,
    {noreply, State}.

