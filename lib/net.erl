-module(net).
-export([start/1, cell/3]).

% XXX here we go ...
%
% Stimulate IF style network:
% - a vanilla message is a quanta
% - we need some type of IO
%
% Build network using R&D style:
% - wtf?
% - OK, any bottom up hack will do for now

start(code) ->
	register(net, spawn(net, cell, [[], [], [code]])).

% Cells:
% - every cell reacts to some type of environmental stimuli
% - every cell has physical neighbors for sending local messages
% - some cells can broadcast global messages 
% - some cells send messages via connections to non-neighbors
cell(Neighbors, Connections, State) ->
	% prehook, act on state
	% message handeling
	receive
		{print, Term} -> 
			io:write(Term), 
			cell(Neighbors, Connections, State);
		stop -> ok
	end.
	% posthook, act on state
