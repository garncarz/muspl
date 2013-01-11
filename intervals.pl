:- module(intervals, [
	intervalDiff/3,
	intervalModDiff/3
	]).

:- ['intervals.plt'].

dbToneToIntervalToC(c, (0, 0)).
dbToneToIntervalToC(ces, (0, -1)).
dbToneToIntervalToC(cis, (0, 1)).

dbToneToIntervalToC(d, (2, 0)).
dbToneToIntervalToC(des, (2, -1)).
dbToneToIntervalToC(dis, (2, 1)).

dbToneToIntervalToC(e, (4, 0)).
dbToneToIntervalToC(es, (4, -1)).
dbToneToIntervalToC(eis, (4, 1)).

dbToneToIntervalToC(f, (5, 0)).
dbToneToIntervalToC(fes, (5, -1)).
dbToneToIntervalToC(fis, (5, 1)).

dbToneToIntervalToC(g, (7, 0)).
dbToneToIntervalToC(ges, (7, -1)).
dbToneToIntervalToC(gis, (7, 1)).

dbToneToIntervalToC(a, (9, 0)).
dbToneToIntervalToC(as, (9, -1)).
dbToneToIntervalToC(ais, (9, 1)).

dbToneToIntervalToC(b, (11, 0)).
dbToneToIntervalToC(bes, (11, -1)).
dbToneToIntervalToC(bis, (11, 1)).


toneToIntervalToC(Tone, (Base, Detail)) :-
	dbToneToIntervalToC(Tone, (Base, Detail)).
toneToIntervalToC((Tone, Octave), (Base, Detail)) :-
	integer(Octave),
	dbToneToIntervalToC(Tone, (Base1, Detail)), !,
	Base is Octave * 12 + Base1.
toneToIntervalToC((Tone, Octave), Int) :-
	integer(Int),
	Int1 is Int mod 12,
	(dbToneToIntervalToC(Tone, (Int1, 0));
		Int2 is Int1 - 1, dbToneToIntervalToC(Tone, (Int2, 1));
		Int2 is Int1 + 1, dbToneToIntervalToC(Tone, (Int2, -1))),
	Octave is Int div 12.
toneToIntervalToC((Tone, Octave), (Base, Detail)) :-
	integer(Base), integer(Detail),
	Base1 is Base mod 12,
	dbToneToIntervalToC(Tone, (Base1, Detail)),
	Octave is Base div 12.

negInterval(Int1, Int2) :-
	integer(Int1),
	Int2 is -Int1.
negInterval((Base1, Detail1), (Base2, Detail2)) :-
	integer(Base1), integer(Detail1),
	Base2 is -Base1, Detail2 is -Detail1.

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
	toneToIntervalToC(Tone2, Int2),
	(atom(Tone1); not(atom(Tone2))).
intervalDiff(Tone1, Tone2, Diff) :-
	var(Tone1), nonvar(Tone2), integer(Diff),
	negInterval(Diff, Diff2),
	intervalDiff(Tone2, Tone1, Diff2).

intervalModDiff(Tone1, Tone2, Diff) :-
	number(Diff),
	intervalDiff(Tone1, Tone2, Diff1),
	Diff mod 12 =:= Diff1 mod 12.
intervalModDiff(Tone1, Tone2, Diff) :-
	intervalDiff(Tone1, Tone2, Diff1),
	Diff is Diff1 mod 12.

