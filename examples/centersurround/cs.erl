-module(cs).

-export([test_simple/0, test_tonic/0, test_cs/2]).
-export([start_feedback/3, start_feedforward/3, signal/1, harness/1]).

-import(record, [cell/1]).
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

% horz. cell acts on bipolar, linear
start_feedforward(Gang, C, S) ->
	E=#exemplar{},
	% create bipolar cell
	Pid_B = record:start_cell((E#exemplar.bi)#state{cpids=[Gang]}),
	% create horizontal cell (these dudes are weird)
	Pid_H = record:start_cell((E#exemplar.horz)#state{cpids=[Pid_B]}),
	% create cones 
	LPid_C = [ record:start_cell((E#exemplar.cone)#state{cpids=[Pid_B]}) | [ record:start_cell((E#exemplar.cone)#state{cpids=[Pid_H]}) || _X <- lists:seq(1,6) ] ],
	% create signals
	LPid_S = [ record:start_cell((E#exemplar.sig)#state{cpids=[lists:nth(1,LPid_C)], trans={inhib, C}}) | [ record:start_cell((E#exemplar.sig)#state{cpids=[lists:nth(X,LPid_C)], trans={inhib, S}}) || X <- lists:seq(2,7) ] ],
	All = [ Pid_B | LPid_C ] ++ [ Pid_H | LPid_S ],
	{Pid_B, Pid_H, LPid_C, LPid_S, All}.

% horz. cell acts on receptors, nonlinear
start_feedback(Gang, C, S) ->
	% create bipolar cell
	Pid_B = record:start_cell(#state{cpids=[Gang], dynamics={tonic, 1}, freq=400, thresh=10}),
	% create horizontal cell (these dudes are weird)
	Pid_H = record:start_cell(#state{thresh=6, trans={inhib, -127}}),
	% create cones 
	LPid_C = [ 
		record:start_cell(#state{cpids=[Pid_B], dynamics={tonic, 25.5}, freq=200, thresh=127, trans={inhib, -30}}) | 
		[ record:start_cell(#state{cpids=[Pid_H], dynamics={tonic, 25.5}, freq=200, thresh=127, trans={excite, 1}}) || _X <- lists:seq(1,6) ]
		],
	Pid_H ! {connect, LPid_C},
	% create signals
	LPid_S = [
		record:start_cell(#state{cpids=[lists:nth(1,LPid_C)], dynamics={tonic, 1}, freq=20, thresh=1, trans={inhib, C}}) | 
		[ record:start_cell(#state{cpids=[lists:nth(X,LPid_C)], dynamics={tonic, 1}, freq=20, thresh=1, trans={inhib, S}}) || X <- lists:seq(2,7) ]
		],
	All = [ Pid_B | LPid_C ] ++ [ Pid_H | LPid_S ],
	{Pid_B, Pid_H, LPid_C, LPid_S, All}.

test_cs_(F, C, S) -> 
	Har = spawn_link(?MODULE, harness, [0]),
	{_,_,_,_, All} = F(Har, C, S),
	timer:send_after(5010, Har, {check, self()}),
	receive 
		Count -> io:format("Cen ~4w Sur ~4w :: ~4w per sec~n", [C, S, Count/5]) 
	end,
	[ P ! stop || P <- All ].

test_cs(F,R) ->
	[ test_cs_(F, C, S) || C <- R, S <- R ],
	ok.

signal([C|S]=CS) ->
	receive 
		{X,Y} -> 
			C ! {trans, {inhib, X}}, 
			[ P ! {trans, {inhib, Y}} || P <- S ], 
			signal(CS);
		stop -> ok
	end.

harness(Count) ->
	receive
		{t, {_Trans, Str}, _Pid} -> harness(Count+Str);
		{check, Pid} -> Pid ! Count
	end.
