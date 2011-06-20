-module(record).
-export([start_cell/1, cell/1]).
-include("record.hrl").

start_cell(S) ->
	spawn_link(?MODULE, cell, [S]).

cell(S = #state{dynamics = {_Type, Str}}) ->
	receive
		{connect, Pids} -> cell(S#state{cpids = S#state.cpids ++ Pids});
		{{_InTrans, InStr}, _Time} -> cell(fire(S#state{count = S#state.count + InStr}));
		stop -> ok
	after 
		round(1000/S#state.freq) -> cell(fire(S#state{count = S#state.count + Str}))
	end.

fire(S) when S#state.count >= S#state.thresh ->
	[ P ! {S#state.trans, time()} || P <- S#state.cpids ], S#state{count=0};
fire(S) ->
	S#state{count = max(S#state.count, 0)}.
