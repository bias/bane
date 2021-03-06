-module(hexagonal).

% Client
-export([start/1, stop/0, info/0, data/0, data/1, morph/0]).
% Backend (don't touch!)
-export([data_collate/2, father/2, aunt/2, uncle/1, mother_cell/1, cell/7]).

-import(record, [cell/1, start_cell/1]).
-include("record.hrl").

-vsn(0.4).

%%
%% Client interface
%%

start(N) ->
	MPid = spawn_link(?MODULE, mother_cell, [trunc(N*(N-1)/2)]),
	register(mother, MPid),
	register(father, spawn_link(?MODULE, father, [[{MPid,0,0}], N])),
	register(uncle, spawn_link(?MODULE, uncle, [[]])),
	register(aunt, spawn_link(?MODULE, aunt, [[], 0])),
	ok.

stop() -> nope.
	
info() ->
	father ! {size_q, self()},
	receive
		{size_r, ActualSize, Size} -> ok
	end,
	{ActualSize, Size}.

morph() ->
	father ! {morph, feedforward}.

data() ->
	Pid = spawn_link(?MODULE, data_collate, [self(), standard_io]),
	aunt ! {c_q, Pid}.

data(FileName) ->
	{ok, Io} = file:open(FileName, write),
	Pid = spawn_link(?MODULE, data_collate, [self(), Io]),
	aunt ! {c_q, Pid}.

%%
%% Backend 
%%

data_collate(RPid, Io) ->
	receive
		% NOTE we could recur with N and solve async issue ... (for you future folks)
		{c_r_start, _N} -> data_collate(RPid, Io);
		{c_r, {{S,T},N}} -> 
			io:format(Io, "add_edge({:time=>~p}, {:pid=>'~p'}, {:pid=>'~p'})~n", [N,S,T]), 
			data_collate(RPid, Io);
		{c_r, {P,N}} -> 
			io:format(Io, "add_vertex({:pid=>'~p',:time=>~p})~n", [P,N]), 
			data_collate(RPid, Io);
		% XXX let's assume we don't have an asynchronicity issue
		{c_r_stop} -> ok
	end.

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

aunt(Creation, T) ->
	receive
		{p_d, Pid} -> aunt([{Pid,T}|Creation], T+1);
		{e_d, {SPid,TPid}} -> aunt([{{SPid,TPid},T}|Creation], T+1);
		{c_q, RPid} -> 
			% XXX again, deal with async later ...
			RPid ! {c_r_start, T},
			[ RPid ! {c_r, Data} || Data <- Creation ],
			RPid ! {c_r_stop},
			aunt(Creation, T)
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

%
% Network generation
%

% Cell Parameters: 
%	Nbh			neighborhood
%	PN			preferred neighbor
%	PNOP		preferred neighbor of parent	
%	P			number of preferences
%	N			layer number (distance from center)
%	Family		family (spiral branch)
%	Gen			family generation (distance along branch)

% Message command prefixes:
%				you have ...
%	pf			been preferred
%	pr			a preference number reply
%	pq			a preference number query
%	nbh			some new neighbors	
%	pnb			a new preferred neighbor
%	nbhq		a neighborhood query

mother_cell(N) -> 
	PidL = [ spawn_link(?MODULE, cell, [[self()], null, null, 0, N, Family, 1]) || Family <- lists:seq(1,6) ],
	[ father ! {p_d, {lists:nth(F,PidL), F, 1}} || F <- lists:seq(1,6) ], 
	aunt ! {p_d, self()},
	[ aunt ! {p_d, lists:nth(F,PidL)} || F <- lists:seq(1,6) ], 
	[ aunt ! {e_d, {self(), lists:nth(F,PidL)}} || F <- lists:seq(1,6) ], 
	[ lists:nth(X, PidL) ! {nbh, [lists:nth(ring(X-1,6), PidL), lists:nth(ring(X+1,6), PidL)]} || X <- lists:seq(1, 6) ],
	[ lists:nth(X, PidL) ! {pnb, lists:nth(ring(X+1,6), PidL)} || X <- lists:seq(1, 6) ],
	neuron(PidL, null, null, 0, null, 0, 0).

cell(Nbh, PN, PNOP=null, P=0, N, Family, Gen) -> 
	receive
		{pf, Pid} -> 
			aunt ! {e_d, {Pid, self()}},
			cell(merge([Pid], Nbh), PN, PNOP, P+1, N, Family, Gen);
		{nbh, NewNbh} ->
			[ aunt ! {e_d, {self(), Target}} || Target <- NewNbh ],
			cell( NewNbh ++ Nbh, PN, PNOP, P, N, Family, Gen);
		{pnb, NewPN} -> 
			NewPN ! {pf, self()},
			cell(Nbh, NewPN, PNOP, P, N, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, Pid} ->
			Pid ! {nbhr, self(), Nbh},
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
	CPid = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1, Family, Gen+1]),
	father ! {p_d, {CPid, Family, Gen+1}},
	aunt ! {p_d, CPid},
	aunt ! {e_d, {self(), CPid}},
	receive
		{pf, Pid} -> 
			aunt ! {e_d, {self(), Pid}},
			neuron(merge([Pid, CPid], Nbh), PN, PNOP, P+1, CPid, Family, Gen);
		% XXX pq - prefernce number query, pr - preference number reply
		{pq, Pid} ->
			aunt ! {e_d, {self(), CPid}},
			Pid ! {pr, P, CPid, self()},
			neuron(merge([Pid, CPid], Nbh), PN, PNOP=null, P, CPid, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, Pid} ->
			Pid ! {nbhr, self(), Nbh},
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
		{pf, Pid} ->
			aunt ! {e_d, {self(), Pid}},
			cell(merge([Pid],Nbh), PN, PNOP, P+1, N, Family, Gen);
		{pr, 1, _CPid, Pid} -> 
			aunt ! {e_d, {self(), Pid}},
			Pid ! {pf, self()},
			cell(merge([Pid], Nbh), Pid, PNOP, P, N, Family, Gen);
		{pr, 2, CPid, Pid} -> 
			aunt ! {e_d, {self(), Pid}},
			aunt ! {e_d, {self(), CPid}},
			CPid ! {pf, self()},
			cell(merge([Pid, CPid], Nbh), CPid, PNOP, P, N, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, Pid} ->
			Pid ! {nbhr, self(), Nbh},
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
	CPid = spawn_link(?MODULE, cell, [[self(), PN], null, PN, 0, N-1, Family, Gen+1]),
	father ! {p_d, {CPid,Family,Gen+1}},
	aunt ! {p_d, CPid},
	receive
		{pf, Pid} -> 
			aunt ! {e_d, {self(), Pid}},
			neuron(merge([Pid, CPid], Nbh), PN, PNOP, P+1, CPid, Family, Gen);
		% XXX pq - prefernce number query, pr - preference number reply
		{pq, Pid} ->
			Pid ! {pr, P, CPid, Pid},
			neuron(merge([Pid, CPid], Nbh), PN, PNOP, P, CPid, Family, Gen);
		% XXX we shouldn't be here 
		{nbhq, Pid} ->
			Pid ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, CPid, Family, Gen);
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

neuron(Nbh, PN, PNOP, P, CPid, Family, Gen) ->
	receive
		{pf, Pid} -> 
			aunt ! {e_d, {self(), Pid}},
			neuron(merge([Pid], Nbh), PN, PNOP, P+1, CPid, Family, Gen);
		{pq, Pid} ->
			Pid ! {pr, P, CPid, self()},
			neuron([Pid | Nbh], PN, PNOP, P, CPid, Family, Gen);
		{nbhq, Pid} ->
			Pid ! {nbhr, self(), Nbh},
			neuron(Nbh, PN, PNOP, P, CPid, Family, Gen);
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

% Multiplicative
ring(0, N) -> N;
ring(M, N) when M == N+1 -> 1;
ring(X, _N) -> X.
