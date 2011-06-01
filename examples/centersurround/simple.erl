-module(simple).
-export([start/0, start_cell/3, cell/4, start_signal/2, signal/2]).

start() ->
	CellPID = start_cell(self(), 60, on),
	{CellPID, start_signal(CellPID, 60)}.

start_cell(PID, Thresh, Type) ->
	spawn_link(?MODULE, cell, [PID, Thresh, Type, 0]).

% XXX for now we are creating different functions for different behaviours :(
cell(PID, Thresh, on, Count) ->
	receive
		{hi, _time} -> 
			NCount = Count + 1,
			fire(NCount, PID, Thresh, off, fun cell/4);
		stop -> ok
	end;
cell(PID, Thresh, off, Count) ->
	receive
		{hi, _time} -> 
			NCount = Count - 1,
			fire(NCount, PID, Thresh, off, fun cell/4);
		stop -> ok
	after 
		% XXX OK - these guys need to be disinhibited
		% and this is an arbitrary cludge to do it (at 60Hz)
		round(1000/60) -> 
			NCount = Count + 1,
			fire(NCount, PID, Thresh, off, fun cell/4)
	end.

start_signal(PID, Freq) ->
	spawn_link(?MODULE, signal, [PID, Freq]).

signal(PID, 0) ->
	receive
		{freq, NFreq} -> signal(PID, NFreq);
		stop -> ok
	end;
signal(PID, Freq) ->
	receive
		{freq, NFreq} -> signal(PID, NFreq);
		stop -> ok
	after
		round(1000/Freq) ->
			PID ! {hi, time()},
			signal(PID, Freq)
	end.

fire(_Count, PID, Thresh, Type, Fun) when _Count > Thresh ->
	PID ! {hi, time()}, 
	Fun(PID, Thresh, Type, 0);
fire(Count, PID, Thresh, Type, Fun) ->
	PID ! {hi, time()}, 
	Fun(PID, Thresh, Type, max(Count,0)).
