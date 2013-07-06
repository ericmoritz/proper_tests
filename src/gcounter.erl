-module(gcounter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, {replicas, current}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, inc/0, inc/1, value/0, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

inc() ->
    inc(1).

inc(N) ->
    gen_server:call(?SERVER, {inc, N}).

value() ->
    gen_server:call(?SERVER, value).    

stop() ->
    gen_server:call(?SERVER, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{replicas=dict:new(), current=1}}.

handle_call({inc, N}, _From, State=#state{replicas=Rs, current=C}) ->
    Replica = replica(C, Rs),
    Rs2 = replica(C, Replica + N, Rs),
    {reply, ok, State#state{replicas=Rs2, current=next_replica(C, 3)}};
handle_call(value, _From, State=#state{replicas=Rs}) ->
    V = value(Rs),
    {reply, V, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
replica(N, Replicas) ->
    case dict:find(N, Replicas) of
	{ok, R} ->
	    R;
	error ->
	    0
    end.

replica(N, V, Replicas) ->
    dict:store(N, V, Replicas).

value(Replicas) ->
    dict:fold(
      fun(_, X, Acc) -> X + Acc end,
      0,
      Replicas
     ).

next_replica(Current, ReplicaCount) when Current >= ReplicaCount ->
    1;
next_replica(Current, _) ->
    Current + 1.
