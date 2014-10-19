:- begin_tests(scale, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(has) :-
	scale{root:c, quality:major}.has(tone{pitch:g}),
	scale{root:d, quality:minor}.has(tone{pitch:bes}),
	not(scale{root:c, quality:major}.has(tone{pitch:es})),
	scale{root:d, quality:minor}.has([tone{pitch:e}, tone{pitch:g, octave:1},
		tone{pitch:c, octave:(-1)}]),
	not(scale{root:c, quality:major}.has([tone{pitch:es}, tone{pitch:g}])).

test(scaleAt) :-
	scaleAt([d, e, f], 5, Tone1), Tone1 == e,
	scaleAt([d, e, f], Index2, Tone2), Index2 == 3, Tone2 == f.

test(scaleToneF, [nondet]) :-
	scaleToneF(Scale1, tone{pitch:b, octave:2}, 1),
		Scale1 == scale{root:c, quality:major},
	scaleToneF(Scale2, tone{pitch:b, octave:2}, 0),
		Scale2 == scale{root:d, quality:minor},
	not(scaleToneF(scale{root:c, quality:major}, tone{pitch:b, octave:2}, 0)).

test(scaleChordF, [nondet]) :-
	scaleChordF(Scale1, [tone{pitch:c}, tone{pitch:d}, tone{pitch:e}], 1),
		Scale1 == scale{root:c, quality:major},
	scaleChordF(Scale2, [tone{pitch:c}, tone{pitch:d}, tone{pitch:es},
		tone{pitch:f}], 0.75), Scale2 == scale{root:d, quality:minor},
	not(scaleChordF(scale{root:c, quality:major},
		[tone{pitch:d}, tone{pitch:bes}], 1)).

test(scaleSongF, [setup(testSong), cleanup(clear)]) :-
	once((scaleSongF(Scale1, Fuzzy1), Scale1 == scale{root:d, quality:minor})),
	Fuzzy1 < 0.9, Fuzzy1 > 0.8,
	once((scaleSongF(Scale2, Fuzzy2),
		Scale2 == scale{root:gis, quality:minor})),
	Fuzzy2 < 0.3.

test(sortedSongScales, [setup(testSong), cleanup(clear)]) :-
	sortedSongScales(Scales),
	nth1(1, Scales, (scale{root:f, quality:major}, _)).

:- end_tests(scale).

