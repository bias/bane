-module(simple).
-export([start/0, start_cell/2, cell/3, start_signal/2, signal/2]).

start() ->
	CellPID = start_cell(self(), 60),
	{CellPID, start_signal(CellPID, 60)}.

start_cell(PID, Thresh) ->
	spawn_link(?MODULE, cell, [PID, Thresh, 0]).

cell(PID, Thresh, Count) ->
	receive
		{hi, _time} -> 
			NCount = Count + 1,
			if 
				NCount > Thresh -> 
					PID ! {hi, time()}, cell(PID, Thresh, 0);
				true ->
					cell(PID, Thresh, NCount)
			end;
		stop -> ok
	end.

start_signal(PID, Freq) ->
	spawn_link(?MODULE, signal, [PID, Freq]).

signal(PID, Freq) ->
	receive
		{freq, NFreq} -> signal(PID, NFreq);
		stop -> ok
	after
		round(1000/Freq) ->
			PID ! {hi, time()},
			signal(PID, Freq)
	end.
