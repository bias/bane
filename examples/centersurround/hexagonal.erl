-module(hexagonal).
-export([start/1, stop/0, tofile/1, info/0]).
-export([mathplot/1, father/1, mother_cell/1, cell/5]).
-vsn(0.0).

%%
%% Client interface
%%

start(N) ->
	MPid = spawn_link(?MODULE, mother_cell, [trunc(N*(N-1)/2)]),
	register(mother, MPid),
	FPid = spawn_link(?MODULE, father, [[MPid]]),
	register(father, FPid),
	{MPid, FPid}.

stop() ->
	father ! {procq, self()},
	receive
		{procr, Pidl} -> [ P ! stop || P <- Pidl ]
	end,
	father ! stop.
	
tofile(Filename) ->
	register(tofile, spawn_link(?MODULE, mathplot, [Filename])).

info() ->
	father ! {sizeq, self()},
	receive
		{sizer, Size} -> ok
	end,
	Size.

%%
%% Backend 
%%

father(Pidl) ->
	receive
		{proc, NPid} -> father(merge(Pidl, [NPid]));
		{procl, NPidl} -> father(merge(Pidl, NPidl));
		{procq, RPid} -> RPid ! {procr, Pidl}, father(Pidl);
		{sizeq, RPid} -> RPid ! {sizer, length(Pidl)}, father(Pidl);
		stop -> ok
	end.

mother_cell(N) -> 
	PIDL = [ spawn_link(?MODULE, cell, [[self()], null, null, 0, N]) || _X <- lists:seq(1,6) ],
	father ! {procl, PIDL},
	[ lists:nth(X, PIDL) ! {nbh, [lists:nth(rollover(X-1), PIDL), lists:nth(rollover(X+1), PIDL)]} || X <- lists:seq(1, 6) ],
	[ lists:nth(X, PIDL) ! {pnb, lists:nth(rollover(X+1), PIDL)} || X <- lists:seq(1, 6) ],
	neuron(PIDL, null, null, 0, null).

cell(Nbh, PN, PNOP=null, P=0, N) -> 
	receive
		{pf, PID} -> 
			cell(merge([PID], Nbh), PN, PNOP, P+1, N);
		{nbh, NewNbh} -> 
			cell( NewNbh ++ Nbh, PN, PNOP, P, N);
		{pnb, NewPN} -> 
			NewPN ! {pf, self()},
			cell(Nbh, NewPN, PNOP, P, N);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, null);
		stop -> ok
	end;

cell(Nbh, PN, PNOP=null, P=1, N) -> 
	CPID = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1]),
	father ! {proc, CPID},
	receive
		{pf, PID} -> 
			neuron(merge([PID, CPID], Nbh), PN, PNOP, P+1, CPID);
		% XXX pq - prefernce number query, pr - preference number reply
		{pq, PID} ->
			PID ! {pr, P, CPID, self()},
			neuron(merge([PID, CPID], Nbh), PN, PNOP=null, P, CPID);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, null);
		stop -> ok
	end;

cell(Nbh, PN=null, PNOP, P, N) ->
	PNOP ! {pq, self()},
	receive
		{pf, PID} ->
			cell(merge([PID],Nbh), PN, PNOP, P+1, N);
		{pr, 1, _CPID, PID} -> 
			PID ! {pf, self()},
			cell(merge([PID], Nbh), PID, PNOP, P, N);
		{pr, 2, CPID, PID} -> 
			CPID ! {pf, self()},
			cell(merge([PID, CPID], Nbh), CPID, PNOP, P, N);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, null);
		stop -> ok
	end;

cell(Nbh, PN, PNOP, P, 0) -> neuron(Nbh, PN, PNOP, P, null);

cell(Nbh, PN, PNOP, P, N) ->
	CPID = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1]),
	father ! {proc, CPID},
	receive
		{pf, PID} -> 
			neuron(merge([PID, CPID], Nbh), PN, PNOP, P+1, CPID);
		% XXX pq - prefernce number query, pr - preference number reply
		{pq, PID} ->
			PID ! {pr, P, CPID, PID},
			neuron(merge([PID, CPID], Nbh), PN, PNOP, P, CPID);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, CPID);
		stop -> ok
	end.

neuron(Nbh, PN, PNOP, P, CPID) ->
	receive
		{pf, PID} -> 
			neuron(merge([PID], Nbh), PN, PNOP, P+1, CPID);
		{pq, PID} ->
			PID ! {pr, P, CPID, self()},
			neuron([PID | Nbh], PN, PNOP, P, CPID);
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, CPID);
		stop -> ok
	end.

merge([], L2) -> L2;

merge([Head|Tail], L2) ->
	case lists:member(Head, L2) of
		true -> merge(Tail, L2);
		false -> merge(Tail, [Head | L2])
	end.

rollover(0) -> 6;
rollover(7) -> 1;
rollover(X) -> X.

mathplot(Filename) ->
	father ! {procq, self()},
	receive
		{procr, Pidl} -> ok 
	end,
	file:write_file(Filename, io_lib:format("GraphPlot[\n{", [])),
	[mathgather(Filename, P) || P <- Pidl], 
	file:write_file(Filename, io_lib:format("}\nVertexLabeling->True,\nDirectedEdges->True,\n]", []), [append]).
		
mathgather(Filename, Pid) ->
	Pid ! {nbhq, self()},
	receive
		{nbhr, Pid, Nbh} ->
			file:write_file(Filename, [ mathedge(Pid, N) || N <- Nbh ], [append]) 			
	end.

mathedge(P, N) ->
	io_lib:format("\"~w\" -> \"~w\", ", [P, N]).
