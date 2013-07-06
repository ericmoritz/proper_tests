-module(counter_proper).

-include_lib("proper/include/proper.hrl").
-behaviour(proper_statem).
-define(SERVER, gcounter).

-export([run/0, command/1, initial_state/0, next_state/3, postcondition/3, precondition/2]).

initial_state() -> 0.

command(_State) ->
    oneof([
	   {call, ?SERVER, inc, [pos_integer()]},
	   {call, ?SERVER, value, []}
	  ]).

next_state(Count, _V, {call, _, inc, [N]}) ->
    Count + N;
next_state(Count, _V, {call, _, value, []}) ->
    Count.


precondition(_S, _Call) -> true.

postcondition(_, {call, _, inc, [N]}, ok) ->
    true; % ignore the state, the value call will verify it 
postcondition(Count, {call, _, value, []}, Count2) ->
    Count == Count2.

prop() ->
    ?FORALL(Cmds, commands(?MODULE),
      ?TRAPEXIT(
	 begin
	     ?SERVER:start_link(),
	     {History,State,Result} = run_commands(?MODULE, Cmds),
	     ?SERVER:stop(),
	     ?WHENFAIL(
		io:format("History: ~w\nState: ~w\nResult: ~w\n",
			  [History,State,Result]),
		aggregate(
		  command_names(Cmds),
		  Result =:= ok))
	 end)).


    
run() ->
    proper:quickcheck(prop()).
