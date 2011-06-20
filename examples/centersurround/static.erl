-module(static).
-export([start/0, all/2, center/2, surround/2]).
-import(simple, [start_cell/2, start_signal/2]).

% XXX I don't know what the tonic firing rate of rods/cones are (I'll dig into it later)
% XXX also, this is a bit off since photoreceptors don't fire per se
start() ->
	%% create bipolar cell
	PID_B = simple:start_cell([self()], 60, inhib),
	%% create horizontal cell (these dudes are weird)
	PID_H = simple:start_cell([PID_B], 60*6, excite),
	%% create cones 
	LPID_C = [ simple:start_cell([PID_B], 60) ] ++  [ simple:start_cell([PID_H], 60) || _X <- lists:seq(1,6) ],
	%% create signals
	_LPID_S = [ simple:start_signal([lists:nth(X,LPID_C)], 60) || X <- lists:seq(1,7) ].

all(LPID, NFreq) ->
	[ P ! {freq, NFreq} || P <- LPID ].

center([Head|_Tail], NFreq) ->
	Head ! {freq, NFreq}.

surround([_Head|Tail], NFreq) ->
	[ P ! {freq, NFreq} || P <- Tail ].
