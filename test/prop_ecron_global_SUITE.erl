-module(prop_ecron_global_SUITE).
-include_lib("ecron/include/ecron.hrl").

%%% Common Test includes
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1]).
-compile(export_all).

-define(Master, 'master@127.0.0.1').
-define(Slave1, 'slave1@127.0.0.1').
-define(Slave2, 'slave2@127.0.0.1').

suite() ->
    [{timetrap, {minutes, 3}}].

all() ->
    [
        basic, quorum, quorum_in_majority, transfer, error_config
    ].

groups() -> [].

init_per_suite(Config) ->
    error_logger:tty(false),
    application:stop(ecron),
    net_kernel:start([?Master, longnames]),
    Config.

end_per_suite(_Config) ->
    error_logger:tty(true),
    ok.

basic(_Config) ->
    start_master(2),
    undefined = global:whereis_name(ecron),
    start_slave(?Slave1, 2),
    timer:sleep(180),
    Pid = global:whereis_name(ecron),
    true = is_pid(Pid),
    timer:sleep(180),
    Pid1 = rpc:call(?Slave1, global, whereis_name, [ecron]),
    true = is_pid(Pid1),
    start_slave(?Slave2, 2),
    timer:sleep(180),
    Pid2 = rpc:call(?Slave2, global, whereis_name, [ecron]),
    true = is_pid(Pid2),
    stop_slave(?Slave1),
    timer:sleep(180),
    Pid3 = global:whereis_name(ecron),
    true = is_pid(Pid3),
    stop_slave(?Slave2),
    timer:sleep(180),
    undefined = global:whereis_name(ecron),
    stop_master(),
    undefined = global:whereis_name(ecron),
    ok.

quorum(_Config) ->
    start_master(1),
    timer:sleep(150),
    Pid0 = global:whereis_name(ecron),
    error = gen_server:call(ecron_monitor, test),
    gen_server:cast(ecron_monitor, test),
    true = is_pid(Pid0),
    start_slave(?Slave1, 1),
    Pid = global:whereis_name(ecron),
    true = is_pid(Pid),
    timer:sleep(180),
    Pid1 = rpc:call(?Slave1, global, whereis_name, [ecron]),
    true = is_pid(Pid1),
    start_slave(?Slave2, 1),
    timer:sleep(180),
    Pid2 = rpc:call(?Slave2, global, whereis_name, [ecron]),
    true = is_pid(Pid2),
    stop_slave(?Slave1),
    timer:sleep(180),
    Pid3 = global:whereis_name(ecron),
    true = is_pid(Pid3),
    stop_slave(?Slave2),
    timer:sleep(180),
    Pid4 = global:whereis_name(ecron),
    true = is_pid(Pid4),
    stop_master(),
    undefined = global:whereis_name(ecron),
    ok.

quorum_in_majority(_Config) ->
    start_master(2),
    timer:sleep(150),
    undefined = global:whereis_name(ecron),
    start_slave(?Slave1, 2),
    timer:sleep(150),
    Pid = global:whereis_name(ecron),
    true = is_pid(Pid),
    Pid1 = rpc:call(?Slave1, global, whereis_name, [ecron]),
    true = is_pid(Pid1),
    start_slave(?Slave2, 2),
    timer:sleep(180),
    Pid2 = rpc:call(?Slave2, global, whereis_name, [ecron]),
    true = is_pid(Pid2),
    rpc:call(?Slave1, application, stop, [ecron]),
    timer:sleep(180),
    Pid3 = global:whereis_name(ecron),
    true = is_pid(Pid3),
    rpc:call(?Slave2, application, stop, [ecron]),
    timer:sleep(180),
    undefined = global:whereis_name(ecron),
    stop_slave(?Slave1),
    stop_slave(?Slave2),
    stop_master(),
    ok.

transfer(_Config) ->
    start_master(1),
    start_slave(?Slave1, 1),
    start_slave(?Slave2, 1),
    stop_master(),
    timer:sleep(180),
    Pid1 = global:whereis_name(ecron),
    true = is_pid(Pid1),
    start_master(1),
    {ok, #{node := Node1}} = ecron:statistic(global_job),
    ok = rpc:call(Node1, application, stop, [ecron]),
    Pid2 = global:whereis_name(ecron),
    true = is_pid(Pid2),
    true = (Pid2 =/= Pid1),
    {error, not_found} = ecron:statistic(no_found),
    [#{node := Node2}] = ecron:statistic(),
    true = Node1 =/= Node2,
    stop_slave(?Slave1),
    stop_slave(?Slave2),
    stop_master(),
    ok.

error_config(_Config) ->
    application:set_env(ecron, global_jobs, [{global_job, "* */10 * * * * *", {io_lib, format, ["error"]}}]),
    Reason = application:start(ecron),
    {error,
        {{shutdown,
            {failed_to_start_child,ecron_monitor,
                "invalid_spec: \"* */10 * * * * *\""}},
            {ecron_app,start,[normal,[]]}}} = Reason,
    undefined = global:whereis_name(ecron),
    ok.

-define(Env(Quorum), [
    {adjusting_time_second, 604800},
    {time_zone, local},
    {cluster_quorum_size, Quorum},
    {local_jobs, []},
    {global_jobs, [{global_job, "*/10 * * * * *", {io_lib, format, ["Runs on 0, 15, 30, 45 seconds~n"]}}]}
]).

start_master(Quorum) when is_integer(Quorum) ->
    start_master(?Env(Quorum));
start_master(Env) ->
    ok = set_app_env(Env),
    {ok, _Apps} = application:ensure_all_started(ecron),
    ok.

stop_master() ->
    ok = application:stop(ecron),
    ok.

start_slave(Node, Quorum) ->
    SlaveStr = atom_to_list(Node),
    [NameStr, _IpStr] = string:tokens(SlaveStr, [$@]),
    Name = list_to_atom(NameStr),
    {ok, _N} = test_server:start_node(Name, slave, [{start_cover, true}]),
    ok = rpc:call(Node, code, add_pathsz, [code:get_path()]),
    ok = rpc:call(Node, ?MODULE, set_app_env, [?Env(Quorum)]),
    {ok, _SlaveApps} = rpc:call(Node, application, ensure_all_started, [ecron]),
    ok.

stop_slave(Node) ->
    true = test_server:stop_node(Node),
    ok.

set_app_env(Env) ->
    ok = lists:foreach(fun({Key, Value}) ->
        ok = application:set_env(ecron, Key, Value, [{persistent, true}])
                       end, Env).
