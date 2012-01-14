#!/usr/bin/env escript
%%! -smp enable debug verbose

-import(sky).
-import(sim).

main(_Args) ->
	[ [ trial(100,K,Kp) || K <- lists:seq(Kp,100,10) ] || Kp <- lists:seq(10,100,10) ],
	timer:sleep(2400).

trial(N, K, Kp) ->
	Ns = integer_to_list(N),
	Ks = integer_to_list(K),
	Kps = integer_to_list(Kp),
	{DBL, _UL} = sim:test_u([N, 5, round(N/10), K, Kp, 100, 1]),
	timer:sleep(1800),
	{ok, File} = file:open("data/"++Ns++"_"++Ks++"_"++Kps, [write]),
	sim:logger(DBL, File).
