-module(record).
-export([start_cell/1, cell/1]).
-include("record.hrl").

start_cell(S = #state{dynamics = {steady, _Y}}) ->
	spawn_link(?MODULE, cell, [S]);
start_cell(S = #state{dynamics = {tonic, X}}) ->
	Pid = spawn_link(?MODULE, cell, [S]),
	{ok, TRef} = timer:send_interval(round(1000/S#state.freq), Pid, {t, {timer, X}, null}),
	Pid ! {tref, TRef},
	Pid.

cell(S) ->
	receive
		_Trans={t, {_InTrans, InStr}, _Pid} -> 
			%io:format("~w got ~w with count ~w~n", [self(), Trans, S#state.count]),
			cell(fire(S#state{count = S#state.count + S#state.coef * InStr}));
		{connect, Pids} -> 
			cell(S#state{cpids = S#state.cpids ++ Pids});
		{tref, TRef} -> 
			case S#state.tref of
				null -> cell(S#state{tref=TRef});
				_P -> timer:cancel(S#state.tref), cell(S#state{tref=TRef})
			end;	
		stop -> ok
	end.

fire(S) when S#state.count >= S#state.thresh ->
	%io:format("     bang ~w fired to ~w~n", [self(), S#state.cpids]),
	[ P ! {t, S#state.trans, self()} || P <- S#state.cpids ], S#state{count= S#state.count - S#state.thresh};
fire(S) ->
	% prevent infinite bottoming out
	S#state{count = max(S#state.count, -10*S#state.thresh)}.
