-record(state, 
	{
		cpids = [], 
		thresh = 60,
		freq = 60,
		trans = {excite, 1},
		dynamics = {steady, 0},
		count = 0,
		tref = null
	}).

-record(examples,
	{
		cone = #state{},
		horz = #state{},
		bi = #state{}
	}).
