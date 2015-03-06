:- begin_tests(chord).

% TODO test false sentences

test(has) :-
	chord{root:e, quality:major}.has(tone{pitch:gis}),
	chord{root:e, quality:major}.has(tone{pitch:b}),
	chord{root:g, quality:sus2}.has(tone{pitch:g}),
	chord{root:g, quality:sus2}.has(tone{pitch:d, octave:1}),
	chord{root:d, quality:sus4}.has(tone{pitch:g}),
	chord{root:d, quality:sus4}.has([tone{pitch:a}, tone{pitch:g},
		tone{pitch:a}, tone{pitch:d}]),
	chord{root:c, quality:major9}.has([tone{pitch:c}, tone{pitch:e},
		tone{pitch:g}, tone{pitch:b}, tone{pitch:d}]),
	not(chord{root:e, quality:major}.has(tone{pitch:g})).

test(tones, [nondet]) :-
	chord{}.tones([tone{pitch:f}, tone{pitch:(as)}, tone{pitch:c, octave:1}])
		= chord{root:f, quality:minor},
	chord{}.tones([tone{pitch:f, octave:2}, tone{pitch:a, octave:2},
			tone{pitch:cis, octave:3}]) = chord{root:f, quality:augmented},
	chord{}.tones([tone{pitch:c}, tone{pitch:e}, tone{pitch:g}, tone{pitch:b},
			tone{pitch:d}]) = chord{root:c, quality:major9}.

test(dbQualityIntervalsDiffer, [fail]) :-
	dbQualityIntervals(Quality1, Intervals),
	dbQualityIntervals(Quality2, Intervals),
	Quality1 \= Quality2.

:- end_tests(chord).

