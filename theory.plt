:- begin_tests(theory).

:- use_module(theory).

test(toneFromScale, [nondet]) :-
	toneFromScale(g, (c, major)),
	toneFromScale(bes, Scale), Scale == (d, minor),
	not(toneFromScale(eis, (c, major))).

test(chordFromScale) :-
	chordFromScale([e, (g, 1), (c, -1)], Scale), Scale == (d, minor),
	not(chordFromScale([eig, g], (c, major))).

:- end_tests(theory).

