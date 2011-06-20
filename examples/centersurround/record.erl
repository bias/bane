-module(record).
-export([start_cell/1, cell/1, start_signal/1, signal/1]).

-record(state, {cpids = [], thresh = 60, freq = 60, trans = {excite, 1}, dynamics = {steady, 0}, count = 0}).

start_cell(S) ->
	spawn_link(?MODULE, cell, [S]).

cell(S = #state{dynamics = {_Type, Str}}) ->
	receive
		{_InTrans, InStr} -> cell(fire(S#state{count = S#state.count - InStr}));
		stop -> ok
	after 
		round(1000/S#state.freq) -> cell(fire(S#state{count = S#state.count + Str}))
	end.

start_signal(S) ->
	spawn_link(?MODULE, signal, [S]).

signal(S = #state{dynamics = {_Type, Str}}) -> 
	receive
		{trans, Value} -> signal(S#state{trans=Value})	
	after
		round(1000/S#state.freq) -> signal(fire(S#state{count = S#state.count + Str}))
	end.	

fire(S) when S#state.count > S#state.thresh ->
	[ P ! S#state.trans || P <- S#state.cpids ], 0;
fire(S) ->
	max(S#state.count, 0).
