-module(hexagonal).
-export([start/1, stop/0, info/0]).
-export([father/1, mother_cell/1, cell/7]).
-vsn(0.2).

%%
%% Client interface
%%

start(N) ->
	MPid = spawn_link(?MODULE, mother_cell, [trunc(N*(N-1)/2)]),
	register(mother, MPid),
	FPid = spawn_link(?MODULE, father, [[{MPid,0,0}]]),
	register(father, FPid),
	{MPid, FPid}.

stop() -> nope.
	
info() ->
	father ! {size_q, self()},
	receive
		{size_r, Size} -> ok
	end,
	Size.

%%
%% Backend 
%%

father(FamilyList) ->
	receive
		{p_data, Pdatatup} -> 
			father([Pdatatup|FamilyList]);
		{p_query, RPid} -> 
			[ RPid ! {p_response, Pdatatup} || Pdatatup <- FamilyList ],
			father(FamilyList);
		{size_q, RPid} -> 
			RPid ! {size_r, length(FamilyList)}, 
			father(FamilyList);
		stop -> ok
	end.

mother_cell(N) -> 
	PIDL = [ spawn_link(?MODULE, cell, [[self()], null, null, 0, N, Family, 1]) || Family <- lists:seq(1,6) ],
	[ father ! {p_data, {lists:nth(F,PIDL), F, 1}} || F <- lists:seq(1,6) ], 
	[ lists:nth(X, PIDL) ! {nbh, [lists:nth(rollover(X-1), PIDL), lists:nth(rollover(X+1), PIDL)]} || X <- lists:seq(1, 6) ],
	[ lists:nth(X, PIDL) ! {pnb, lists:nth(rollover(X+1), PIDL)} || X <- lists:seq(1, 6) ],
	neuron(PIDL, null, null, 0, null, 0, 0).

cell(Nbh, PN, PNOP=null, P=0, N, Family, Gen) -> 
	receive
		{pf, PID} -> 
			cell(merge([PID], Nbh), PN, PNOP, P+1, N, Family, Gen);
		{nbh, NewNbh} -> 
			cell( NewNbh ++ Nbh, PN, PNOP, P, N, Family, Gen);
		{pnb, NewPN} -> 
			NewPN ! {pf, self()},
			cell(Nbh, NewPN, PNOP, P, N, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		stop -> ok
	end;

cell(Nbh, PN, PNOP=null, P=1, N, Family, Gen) -> 
	CPID = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1, Family, Gen+1]),
	father ! {p_data, {CPID, Family, Gen+1}},
	receive
		{pf, PID} -> 
			neuron(merge([PID, CPID], Nbh), PN, PNOP, P+1, CPID, Family, Gen);
		% XXX pq - prefernce number query, pr - preference number reply
		{pq, PID} ->
			PID ! {pr, P, CPID, self()},
			neuron(merge([PID, CPID], Nbh), PN, PNOP=null, P, CPID, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		stop -> ok
	end;

cell(Nbh, PN=null, PNOP, P, N, Family, Gen) ->
	PNOP ! {pq, self()},
	receive
		{pf, PID} ->
			cell(merge([PID],Nbh), PN, PNOP, P+1, N, Family, Gen);
		{pr, 1, _CPID, PID} -> 
			PID ! {pf, self()},
			cell(merge([PID], Nbh), PID, PNOP, P, N, Family, Gen);
		{pr, 2, CPID, PID} -> 
			CPID ! {pf, self()},
			cell(merge([PID, CPID], Nbh), CPID, PNOP, P, N, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		stop -> ok
	end;

cell(Nbh, PN, PNOP, P, 0, Family, Gen) -> neuron(Nbh, PN, PNOP, P, null, Family, Gen);

cell(Nbh, PN, PNOP, P, N, Family, Gen) ->
	CPID = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1, Family, Gen+1]),
	father ! {p_data, {CPID,Family,Gen+1}},
	receive
		{pf, PID} -> 
			neuron(merge([PID, CPID], Nbh), PN, PNOP, P+1, CPID, Family, Gen);
		% XXX pq - prefernce number query, pr - preference number reply
		{pq, PID} ->
			PID ! {pr, P, CPID, PID},
			neuron(merge([PID, CPID], Nbh), PN, PNOP, P, CPID, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, CPID, Family, Gen);
		stop -> ok
	end.

neuron(Nbh, PN, PNOP, P, CPID, Family, Gen) ->
	receive
		{pf, PID} -> 
			neuron(merge([PID], Nbh), PN, PNOP, P+1, CPID, Family, Gen);
		{pq, PID} ->
			PID ! {pr, P, CPID, self()},
			neuron([PID | Nbh], PN, PNOP, P, CPID, Family, Gen);
		{nbhq, PID} ->
			PID ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, CPID, Family, Gen);
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
