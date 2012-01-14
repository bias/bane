-record(state, 
	{
		cpids = [], 
		thresh = 60,
		freq = 60,
		trans = {excite, 1},
		dynamics = {steady, 0},
		coef = 1,
		count = 0,
		tref = null
	}).

-record(exemplar,
	{
		bi = #state{dynamics={tonic, 1}, freq=200, thresh=100},
		cone = #state{dynamics={tonic, 25.5}, freq=200, thresh=255, trans={inhib, -200}},
		horz = #state{thresh=6*180, trans={excite, 100}, coef=-1},
		sig = #state{dynamics={tonic, 1}, freq=20, thresh=1}
	}).
