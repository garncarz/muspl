:- ['intervals.plt'].

toneToIntervalToC(c, (0, 0)).
toneToIntervalToC(ces, (0, -1)).
toneToIntervalToC(cis, (0, 1)).

toneToIntervalToC(d, (2, 0)).
toneToIntervalToC(des, (2, -1)).
toneToIntervalToC(dis, (2, 1)).

toneToIntervalToC(e, (4, 0)).
toneToIntervalToC(es, (4, -1)).
toneToIntervalToC(eis, (4, 1)).

toneToIntervalToC(f, (5, 0)).
toneToIntervalToC(fes, (5, -1)).
toneToIntervalToC(fis, (5, 1)).

toneToIntervalToC(g, (7, 0)).
toneToIntervalToC(ges, (7, -1)).
toneToIntervalToC(gis, (7, 1)).

toneToIntervalToC(a, (9, 0)).
toneToIntervalToC(as, (9, -1)).
toneToIntervalToC(ais, (9, 1)).

toneToIntervalToC(b, (11, 0)).
toneToIntervalToC(bes, (11, -1)).
toneToIntervalToC(bis, (11, 1)).

toneToIntervalToC((Tone, Octave), (Base, Detail)) :-
	integer(Octave),
	toneToIntervalToC(Tone, (Base1, Detail)),
	Base is Octave * 12 + Base1.

toneToIntervalToC((Tone, Octave), Int) :-
	integer(Int),
	Int1 is Int mod 12,
	(toneToIntervalToC(Tone, (Int1, 0));
		Int2 is Int1 - 1, toneToIntervalToC(Tone, (Int2, 1));
		Int2 is Int1 + 1, toneToIntervalToC(Tone, (Int2, -1))),
	Octave is Int div 12.


intervalDiff(Int, Int, 0) :- !.

intervalDiff((Base1, Detail1), (Base2, Detail2), Diff) :-
	integer(Base1), integer(Detail1), integer(Base2), integer(Detail2), !,
	Diff is (Base2 + Detail2) - (Base1 + Detail1).
intervalDiff((Base1, Detail1), (Base2, Detail2), Diff) :-
	integer(Base1), integer(Detail1), integer(Diff), !,
	(Detail2 = 0; Detail2 = -1; Detail2 = 1),
	Base2 is Base1 + Detail1 + Diff - Detail2.

intervalDiff(Tone1, Tone2, Diff) :-
	nonvar(Tone1), nonvar(Tone2), !,
	toneToIntervalToC(Tone1, Int1),
	toneToIntervalToC(Tone2, Int2),
	intervalDiff(Int1, Int2, Diff).

intervalDiff(Tone1, Tone2, Diff) :-
	nonvar(Tone1), var(Tone2), integer(Diff),
	toneToIntervalToC(Tone1, Int1),
	intervalDiff(Int1, Int2, Diff),
	toneToIntervalToC(Tone2, Int2).

