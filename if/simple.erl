-module(simple).
-export([start/0, start_cell/3, cell/3, start_signal/2, signal/2]).

start() ->
	CellPID = start_cell(self(), 60, 60),
	{CellPID, start_signal(CellPID, 60)}.

start_cell(PID, Freq, Thresh) ->
	spawn_link(?MODULE, cell, [PID, [Freq, Thresh], 0]).

cell(PID, [Freq, Thresh], Count) ->
	receive
		{hi, _time} -> cell(PID, [Freq, Thresh], Count+1);
		stop -> ok
	after
		round(1000/Freq) -> 
			if 
				Count > Thresh -> 
					PID ! {hi, time()}, cell(PID, [Freq, Thresh], 0);
				true ->
					cell(PID, [Freq, Thresh], Count)
			end
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
