-module(simple).
-export([start/4, start_cell/3, cell/4, start_cone/5, cone/6, start_signal/3, signal/3]).

start(Trans, Freq, Thresh, Str) ->
	CellPID = start_cell([self()], Thresh, Trans),
	{CellPID, start_signal([CellPID], Freq, Str=0)}.

start_cell(LPID, Thresh, Trans) ->
	spawn_link(?MODULE, cell, [LPID, Thresh, Trans, 0]).

% XXX for now we are creating different functions for different behaviours :(
cell(LPID, Thresh, Trans=excite, Count) ->
	receive
		{_InTrans, Str} -> cell(LPID, Thresh, Trans, fire(LPID, Thresh, Trans, Count+Str));
		stop -> ok
	end;
cell(LPID, Thresh, Trans=inhib, Count) ->
	receive
		{_InTrans, Str} -> cell(LPID, Thresh, Trans, fire(LPID, Thresh, Trans, Count-Str));
		stop -> ok
	after 
		% XXX OK - these guys need to be disinhibited
		round(1000/Thresh) -> cell(LPID, Thresh, Trans, fire(LPID, Thresh, Trans, Count+1))
	end.

start_cone(LPID, Thresh, Freq, Str, Trans) ->
	spawn_link(?MODULE, cone, [LPID, Thresh, Freq, Str, Trans, 0]).

cone(LPID, Thresh, Freq, Str, Trans, Count) ->
	receive
		{photon, Intensity} -> cone(LPID, Thresh, Freq, Str, Trans,  fire(LPID, Thresh, Trans, Count - Intensity));
		stop -> ok
	after 
		% XXX tonic firing
		round(1000/Freq) -> cone(LPID, Thresh, Freq, Str, Trans, fire(LPID, Thresh, Trans, Count + Str))
	end.

start_signal(LPID, Freq, Str) ->
	spawn_link(?MODULE, signal, [LPID, Freq, Str]).

signal(LPID, Freq=0, Str) ->
	receive
		{freq, NFreq} -> signal(LPID, NFreq, Str);
		{str, NStr} -> signal(LPID, Freq, NStr);
		stop -> ok
	end;
signal(LPID, Freq, Str) ->
	receive
		{freq, NFreq} -> signal(LPID, NFreq, Str);
		{str, NStr} -> signal(LPID, Freq, NStr);
		stop -> ok
	after
		round(1000/Freq) ->
			[ P ! {photon, Str} || P <- LPID ],
			signal(LPID, Freq, Str)
	end.

fire(LPID, Thresh, Trans, Count) when Count > Thresh ->
	[ P ! {Trans, time()} || P <- LPID ], 0;
fire(_LPID, _Thresh, _Trans, Count) ->
	max(Count, 0).
