-module(stateful_cron_by_send_after).

-behaviour(gen_server).

-export([cancel_weekend_staff/0]).
-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {timer_ref = undefined}).
-define(CrontabSpec, "0 0 20 * * 0,6").

cancel_weekend_staff() ->
    gen_server:call(?MODULE, cancel_weekend_staff).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Ref} = send_after_weekend_msg(),
    {ok, #state{timer_ref = Ref}}.

handle_call(cancel_weekend_staff, _From,
    State = #state{timer_ref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    {reply, ok, State#state{timer_ref = undefined}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(weekend_staff, State) ->
    do_weekend_staff(),
    {ok, Ref} = send_after_weekend_msg(),
    {noreply, State#state{timer_ref = Ref}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% The timer will be automatically canceled
%% if the given dest is a PID which is not alive or when the given PID no-exits.
%% behaviour as erlang:send_after/3
send_after_weekend_msg() ->
    ecron:send_after(?CrontabSpec, self(), weekend_staff).

do_weekend_staff() ->
    io:format("do weekday staff at ~p~n", [erlang:localtime()]).
