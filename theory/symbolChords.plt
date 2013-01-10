:- begin_tests(symbolChords).

test(symbolChord, [nondet]) :-
	symbolChord((e, major), Chord1), Chord1 == [e, gis, b],
	symbolChord(SymbChord2, [f, as, (c, 1)]), SymbChord2 == (f, minor),
	symbolChord(SymbChord3, [(f, 2), (a, 2), (cis, 3)]),
		SymbChord3 == ((f, 2), augmented),
	symbolChord(((c, -1), diminished), Chord4),
		Chord4 == [(c, -1), (es, -1), (ges, -1)],
	symbolChord((g, sus2), [g, a, (d, 1)]),
	symbolChord((d, sus4), [d, g, a]),
	symbolChord((d, sus4), [a, g, a, d]),
	symbolChord(SymbChord5, [c, e, g, b, d]), SymbChord5 == (c, major9).
	

test(symbolChordF) :-
	symbolChordF((c, major), [c, e, g], Fuzzy1),
	symbolChordF((c, major), [g, e, c], Fuzzy2), Fuzzy1 == Fuzzy2,
	symbolChordF((c, major), [g, e, c, e], Fuzzy3), Fuzzy1 == Fuzzy3,
	symbolChordF((c, major), [c, g], Fuzzy4), Fuzzy4 < Fuzzy1,
	symbolChordF((c, major), [c, d, e, g], Fuzzy5), Fuzzy5 < Fuzzy1,
	symbolChordF((c, major), [c, d, e, g, ais], Fuzzy6), Fuzzy6 < Fuzzy5,
	symbolChordF(((e, 0), sus2), [(e, 0), ges, (b, 0)], Fuzzy7),
		Fuzzy7 == Fuzzy1.

test(dbSymbolIntervalsDiffer, [fail]) :-
	dbSymbolIntervals(Quality1, Intervals),
	dbSymbolIntervals(Quality2, Intervals),
	Quality1 \= Quality2.

:- end_tests(symbolChords).

