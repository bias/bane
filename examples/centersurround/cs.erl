-module(cs).
-export([start_simple/0, start_feedback/0, start_feedforward/0, all/2, center/2, surround/2, on/0, off/0, none/0]).
-import(record, [start_cell/1, start_signal/1]).

-include("record.hrl").

start_simple() ->
	Pid = record:start_cell(#state{cpids=[self()]}),
	record:start_cell(#state{cpids=[Pid], dynamics={tonic, 60}}). 

start_feedback() ->
	%% create bipolar cell
	Pid_B = record:start_cell(#state{cpids=[self()], dynamics={tonic, 1}}),
	%% create horizontal cell (these dudes are weird)
	Pid_H = record:start_cell(#state{trans={gaba, -1}}),
	%% create cones 
	LPid_C = [ record:start_cell(#state{cpids=[Pid_B, Pid_H], dynamics={tonic, 60}}) | [ record:start_cell(#state{cpids=[Pid_H]}) || _X <- lists:seq(1,6) ]],
	Pid_H ! {connect, LPid_C},
	%% create signals
	_LPid_S = [ record:start_cell(#state{cpids=[lists:nth(X,LPid_C)], dynamics={tonic, 60}}) || X <- lists:seq(1,7) ].

start_feedforward() -> ok.

all(LPID, NFreq) ->
	[ P ! {freq, NFreq} || P <- LPID ].

center([Head|_Tail], NFreq) ->
	Head ! {freq, NFreq}.

surround([_Head|Tail], NFreq) ->
	[ P ! {freq, NFreq} || P <- Tail ].

on() -> ok.

off() -> ok.

none() -> ok.
