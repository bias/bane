-module(hexagonal).

-export([start/1, stop/0, info/0, morph/0]).
-export([father/2, uncle/1, mother_cell/1, cell/7]).

-import(record, [cell/1, start_cell/1]).
-include("record.hrl").

-vsn(0.3).

%%
%% Client interface
%%

start(N) ->
	MPid = spawn_link(?MODULE, mother_cell, [trunc(N*(N-1)/2)]),
	register(mother, MPid),
	FPid = spawn_link(?MODULE, father, [[{MPid,0,0}], N]),
	register(father, FPid),
	UPid = spawn_link(?MODULE, uncle, [[]]),
	register(uncle, UPid),
	{MPid, FPid}.

stop() -> nope.
	
info() ->
	father ! {size_q, self()},
	receive
		{size_r, ActualSize, Size} -> ok
	end,
	{ActualSize, Size}.

morph() ->
	father ! {morph, feedforward}.

%%
%% Backend 
%%

father(FamilyList, N) ->
	receive
		{p_d, Pdatatup} -> 
			father([Pdatatup|FamilyList], N);
		{p_q, RPid} -> 
			RPid ! {p_r_start, length(FamilyList)},
			[ RPid ! {p_r, Pdatatup} || Pdatatup <- FamilyList ],
			RPid ! {p_r_stop},
			father(FamilyList, N);
		{size_q, RPid} ->
			RPid ! {size_r, length(FamilyList), N*(N-1)*3+7},
			father(FamilyList, N);
		{asize_q, RPid} -> 
			RPid ! {asize_r, length(FamilyList)}, 
			father(FamilyList, N);
		{morph, feedforward} ->
			[ P ! {morph, feedforward} || {P,_,_} <- FamilyList ],
			father(FamilyList, N);
		stop -> ok
	end.

uncle(FamilyList) ->
	receive
		{p_d, Pdatatup} -> 
			uncle([Pdatatup|FamilyList]);
		{p_q, RPid} -> 
			RPid ! {p_r_start, length(FamilyList)},
			[ RPid ! {p_r, Pdatatup} || Pdatatup <- FamilyList ],
			RPid ! {p_r_stop},
			uncle(FamilyList)
	end.

mother_cell(N) -> 
	PIDL = [ spawn_link(?MODULE, cell, [[self()], null, null, 0, N, Family, 1]) || Family <- lists:seq(1,6) ],
	[ father ! {p_d, {lists:nth(F,PIDL), F, 1}} || F <- lists:seq(1,6) ], 
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
		% XXX temporary hack to pass along image
		{t, {pho,Str}, _} ->
			{any, 'visualize@127.0.0.1'} ! {t, {pho, Str}, self()}, neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		% XXX XXX seriously XXX XXX this is getting to be an absurd kludge
		{morph, feedforward} ->
			E = #exemplar{},
			B = record:start_cell((E#exemplar.bi)#state{cpids=[{any, 'visualize@127.0.0.1'}]}),
			uncle ! {p_d, {B, Family, Gen}},
			H = record:start_cell((E#exemplar.horz)#state{cpids=[B]}),
			[ Nh ! {connect, [H]} || Nh <- Nbh],
			record:cell((E#exemplar.cone)#state{cpids=[B]});
		stop -> ok
	end;

cell(Nbh, PN, PNOP=null, P=1, N, Family, Gen) -> 
	CPID = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1, Family, Gen+1]),
	father ! {p_d, {CPID, Family, Gen+1}},
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
		% XXX temporary hack to pass along image
		{t, {pho, Str}, _} ->
			{any, 'visualize@127.0.0.1'} ! {t, {pho, Str}, self()}, neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		% XXX XXX seriously XXX XXX this is getting to be an absurd kludge
		{morph, feedforward} ->
			E = #exemplar{},
			B = record:start_cell((E#exemplar.bi)#state{cpids=[{any, 'visualize@127.0.0.1'}]}),
			uncle ! {p_d, {B, Family, Gen}},
			H = record:start_cell((E#exemplar.horz)#state{cpids=[B]}),
			[ Nh ! {connect, [H]} || Nh <- Nbh],
			record:cell((E#exemplar.cone)#state{cpids=[B]});
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
		% XXX temporary hack to pass along image
		{t, {pho, Str}, _} ->
			{any, 'visualize@127.0.0.1'} ! {t, {pho, Str}, self()}, neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		% XXX XXX seriously XXX XXX this is getting to be an absurd kludge
		{morph, feedforward} ->
			E = #exemplar{},
			B = record:start_cell((E#exemplar.bi)#state{cpids=[{any, 'visualize@127.0.0.1'}]}),
			uncle ! {p_d, {B, Family, Gen}},
			H = record:start_cell((E#exemplar.horz)#state{cpids=[B]}),
			[ Nh ! {connect, [H]} || Nh <- Nbh],
			record:cell((E#exemplar.cone)#state{cpids=[B]});
		stop -> ok
	end;

cell(Nbh, PN, PNOP, P, 0, Family, Gen) -> neuron(Nbh, PN, PNOP, P, null, Family, Gen);

cell(Nbh, PN, PNOP, P, N, Family, Gen) ->
	CPID = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1, Family, Gen+1]),
	father ! {p_d, {CPID,Family,Gen+1}},
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
		% XXX temporary hack to pass along image
		{t, {pho, Str}, _} ->
			{any, 'visualize@127.0.0.1'} ! {t, {pho, Str}, self()}, neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		% XXX XXX seriously XXX XXX this is getting to be an absurd kludge
		{morph, feedforward} ->
			E = #exemplar{},
			B = record:start_cell((E#exemplar.bi)#state{cpids=[{any, 'visualize@127.0.0.1'}]}),
			uncle ! {p_d, {B, Family, Gen}},
			H = record:start_cell((E#exemplar.horz)#state{cpids=[B]}),
			[ Nh ! {connect, [H]} || Nh <- Nbh],
			record:cell((E#exemplar.cone)#state{cpids=[B]});
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
		% XXX temporary hack to pass along image
		{t, {pho, Str}, _} ->
			{any, 'visualize@127.0.0.1'} ! {t, {pho, Str}, self()}, neuron(Nbh, PN, PNOP, P, null, Family, Gen);
		% XXX XXX seriously XXX XXX this is getting to be an absurd kludge
		{morph, feedforward} ->
			E = #exemplar{},
			B = record:start_cell((E#exemplar.bi)#state{cpids=[{any, 'visualize@127.0.0.1'}]}),
			uncle ! {p_d, {B, Family, Gen}},
			H = record:start_cell((E#exemplar.horz)#state{cpids=[B]}),
			[ N ! {connect, [H]} || N <- Nbh],
			record:cell((E#exemplar.cone)#state{cpids=[B]});
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
