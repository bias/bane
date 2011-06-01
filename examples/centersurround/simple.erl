-module(simple).
-export([start/2, start_cell/3, cell/4, start_signal/2, signal/2]).

start(Type, Freq) ->
	CellPID = start_cell([self()], Freq, Type),
	{CellPID, start_signal([CellPID], Freq)}.

start_cell(LPID, Thresh, Type) ->
	spawn_link(?MODULE, cell, [LPID, Thresh, Type, 0]).

% XXX for now we are creating different functions for different behaviours :(
cell(LPID, Thresh, Type=excite, Count) ->
	receive
		{hi, _time} -> fire(Count + 1, LPID, Thresh, Type, fun cell/4);
		stop -> ok
	end;
cell(LPID, Thresh, Type=inhib, Count) ->
	receive
		{hi, _time} -> fire(Count - 1, LPID, Thresh, Type, fun cell/4);
		stop -> ok
	after 
		% XXX OK - these guys need to be disinhibited
		% and this is an arbitrary cludge to do it (at 60Hz)
		round(1000/Thresh) -> fire(Count + 1, LPID, Thresh, Type, fun cell/4)
	end.

start_signal(LPID, Freq) ->
	spawn_link(?MODULE, signal, [LPID, Freq]).

signal(LPID, 0) ->
	receive
		{freq, NFreq} -> signal(LPID, NFreq);
		stop -> ok
	end;
signal(LPID, Freq) ->
	receive
		{freq, NFreq} -> signal(LPID, NFreq);
		stop -> ok
	after
		round(1000/Freq) ->
			[ P ! {hi, time()} || P <- LPID ],
			signal(LPID, Freq)
	end.

fire(_Count, LPID, Thresh, Type, Fun) when _Count > Thresh ->
	[ P ! {hi, time()} || P <- LPID ],
	Fun(LPID, Thresh, Type, 0);
fire(Count, LPID, Thresh, Type, Fun) ->
	Fun(LPID, Thresh, Type, max(Count,0)).
