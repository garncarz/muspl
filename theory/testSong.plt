:- use_module(aux).
:- use_module(data).

clear :- clearData.

ts34 :- assertz(timeSignature(3, 4)).

ts68 :- assertz(timeSignature(6, 8)).

testSong :- multiAssert([
	timeSignature(6, 8),
	
	notation((1, 1, g), (a, 1), 8),
	notation((1, 1, g), (f, 1), 8),
	notation((1, 2, g), (bes, 1), 8),
	notation((1, 3, g), (a, 1), 8),
	notation((1, 3, g), (f, 1), 8),
	notation((1, 4, g), (g, 1), 8),
	notation((1, 4, g), (e, 1), 8),
	notation((1, 5, g), (f, 1), 8),
	notation((1, 5, g), (d, 1), 8),
	notation((1, 6, g), (g, 1), 8),
	notation((1, 6, g), (e, 1), 8),
	
	notation((1, 1, f), (f, 0), 8),
	notation((1, 1, f), (c, 1), 8),
	notation((1, 2, f), (d, 1), 8),
	notation((1, 3, f), (f, 0), 8),
	notation((1, 3, f), (c, 1), 8),
	notation((1, 4, f), (c, 0), 4),
	notation((1, 4, f), (bes, 0), 4),
	notation((1, 6, f), (c, 0), 8),
	notation((1, 6, f), (bes, 0), 8),
	
	notation((4, 3, g), r, 8)]).

