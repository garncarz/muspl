:- begin_tests(symbolChords).

% TODO test false sentences

test(symbolChord, [nondet]) :-
	symbolChord(chord{root:e, quality:major}, Chord1),
		Chord1 == [tone{pitch:e}, tone{pitch:gis}, tone{pitch:b}],
	symbolChord(SymbChord2,
		[tone{pitch:f}, tone{pitch:(as)}, tone{pitch:c, octave:1}]),
		SymbChord2 == chord{root:f, quality:minor},
	symbolChord(SymbChord3,
		[tone{pitch:f, octave:2}, tone{pitch:a, octave:2},
			tone{pitch:cis, octave:3}]),
		SymbChord3 == chord{root:tone{pitch:f, octave:2}, quality:augmented},
	symbolChord(chord{root:tone{pitch:c, octave:(-1)}, quality:diminished},
		Chord4),
		Chord4 == [tone{pitch:c, octave:(-1)}, tone{pitch:es, octave:(-1)},
			tone{pitch:ges, octave:(-1)}],
	symbolChord(chord{root:g, quality:sus2},
		[tone{pitch:g}, tone{pitch:a}, tone{pitch:d, octave:1}]),
	symbolChord(chord{root:d, quality:sus4},
		[tone{pitch:d}, tone{pitch:g}, tone{pitch:a}]),
	symbolChord(chord{root:d, quality:sus4},
		[tone{pitch:a}, tone{pitch:g}, tone{pitch:a}, tone{pitch:d}]),
	symbolChord(SymbChord5,
		[tone{pitch:c}, tone{pitch:e}, tone{pitch:g}, tone{pitch:b},
			tone{pitch:d}]),
		SymbChord5 == chord{root:c, quality:major9}.
	

test(symbolChordF) :-
	symbolChordF(chord{root:c, quality:major},
		[tone{pitch:c}, tone{pitch:e}, tone{pitch:g}], Fuzzy1),
	symbolChordF(chord{root:c, quality:major},
		[tone{pitch:g}, tone{pitch:e}, tone{pitch:c}], Fuzzy2),
		Fuzzy1 == Fuzzy2,
	symbolChordF(chord{root:c, quality:major},
		[tone{pitch:g}, tone{pitch:e}, tone{pitch:c}, tone{pitch:e}], Fuzzy3),
		Fuzzy1 == Fuzzy3,
	symbolChordF(chord{root:c, quality:major},
		[tone{pitch:c}, tone{pitch:g}], Fuzzy4),
		Fuzzy4 < Fuzzy1,
	symbolChordF(chord{root:c, quality:major},
		[tone{pitch:c}, tone{pitch:d}, tone{pitch:e}, tone{pitch:g}], Fuzzy5),
		Fuzzy5 < Fuzzy1,
	symbolChordF(chord{root:c, quality:major},
		[tone{pitch:c}, tone{pitch:d}, tone{pitch:e}, tone{pitch:g},
			tone{pitch:ais}], Fuzzy6),
		Fuzzy6 < Fuzzy5,
	symbolChordF(chord{root:tone{pitch:e, octave:0}, quality:sus2},
		[tone{pitch:e, octave:0}, tone{pitch:ges}, tone{pitch:b, octave:0}],
			Fuzzy7),
		Fuzzy7 == Fuzzy1.

test(dbSymbolIntervalsDiffer, [fail]) :-
	dbSymbolIntervals(Quality1, Intervals),
	dbSymbolIntervals(Quality2, Intervals),
	Quality1 \= Quality2.

:- end_tests(symbolChords).

