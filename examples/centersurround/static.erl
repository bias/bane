-module(static).
-export([start/0]).
-import(simple, [start_cell/2, start_signal/2]).

%
% Ofcourse, this is a bit backwards, but that'll have to do for now
%

% XXX I don't know what the tonic firing rate of rods/cones are (I'll dig into it later)
start() ->
	%% create ganglion cell
	PID_G = simple:start_cell(self(), 60, on),
	%% create bipolar cells
	LPID_B = [ simple:start_cell(PID_G, 6, type(Type)) || X <- lists:seq(1,6), Type <- [X rem 5 == 0] ],
	%% FIXME we need to add lateral inhibition via horizontal cells
	%PID_H = simple:start_cell()
	%% create signals (cones releasing glutamate - excitatory neurotransmitter)
	LPID_S = [ simple:start_signal(lists:nth(X, LPID_B), 60) || X <- lists:seq(1,6) ],
	{PID_G, LPID_B, LPID_S, [PID_G] ++ LPID_B ++ LPID_S}.

type(false) -> off;
type(true) -> on.
