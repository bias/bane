-module(cs).
-export([test_simple/0, test_tonic/0, start_feedback/0, start_feedforward/0, monitor/1]).
-import(record, [start_cell/1, start_signal/1]).

-include("record.hrl").

%%
%% sanity check for record based neurons

% connect tonic to regular cell
test_simple() ->
	Pid = record:start_cell(#state{cpids=[self()]}),
	record:start_cell(#state{cpids=[Pid], dynamics={tonic, 60}}). 

% chain 3 tonics to play with time difference issues
test_tonic() ->
	B = record:start_cell(#state{cpids=[self()], dynamics={tonic, 1}, freq=20, thresh=20}),
	C = record:start_cell(#state{cpids=[B], dynamics={tonic, 25.5}, freq=200, thresh=255, trans={inhib,-1}}),
	S = record:start_cell(#state{cpids=[C], dynamics={tonic, 1}, freq=20, thresh=1, trans={inhib,-255}}),
	{B,C,S}.

%%
%% Center surround models using record based neurons 

% horz. cell acts on receptors, nonlinear
start_feedback() ->
	% create bipolar cell
	Pid_B = record:start_cell(#state{cpids=[self()], dynamics={tonic, 1}}),
	% create horizontal cell (these dudes are weird)
	Pid_H = record:start_cell(#state{trans={gaba, -1}}),
	% create cones 
	LPid_C = [ record:start_cell(#state{cpids=[Pid_B, Pid_H], dynamics={tonic, 6}}) | [ record:start_cell(#state{cpids=[Pid_H]}) || _X <- lists:seq(1,6) ]],
	% connect cones to horz.
	Pid_H ! {connect, LPid_C},
	% create signals
	_LPid_S = [ record:start_cell(#state{cpids=[lists:nth(X,LPid_C)], dynamics={tonic, 6}}) || X <- lists:seq(1,7) ].

% horz. cell acts on bipolar, linear
start_feedforward() ->
	% create bipolar cell
	Pid_B = record:start_cell(#state{cpids=[self()], dynamics={tonic, 1}, freq=20, thresh=20}),
	% create horizontal cell (these dudes are weird)
	Pid_H = record:start_cell(#state{cpids=[Pid_B], thresh=6, trans={excite, 1}}),
	% create cones 
	LPid_C = [ 
		record:start_cell(#state{cpids=[Pid_B], dynamics={tonic, 25.5}, freq=200, thresh=255, trans={inhib, -2}}) | 
		[ record:start_cell(#state{cpids=[Pid_H], dynamics={tonic, 25.5}, freq=200, thresh=255, trans={excite, 1}}) || _X <- lists:seq(1,6) ]
		],
	% create signals
	LPid_S = [ record:start_cell(#state{cpids=[lists:nth(X,LPid_C)], dynamics={tonic, 1}, freq=20, thresh=1, trans={inhib, -255}}) || X <- lists:seq(1,7) ],
	spawn_link(?MODULE, monitor, [LPid_S]).

monitor([C|S]=CS) ->
	receive 
		{X,Y} -> 
			C ! {trans, {inhib, X}}, 
			[ P ! {trans, {inhib, Y}} || P <- S ], 
			monitor(CS)
	end.

