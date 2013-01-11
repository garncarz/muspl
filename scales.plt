:- begin_tests(scales, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(toneFromScale, [nondet]) :-
	toneFromScale(g, (c, major)),
	toneFromScale(bes, Scale), Scale == (d, minor),
	not(toneFromScale(eis, (c, major))).

test(chordFromScale, [nondet]) :-
	chordFromScale([e, (g, 1), (c, -1)], Scale), Scale == (d, minor),
	not(chordFromScale([eig, g], (c, major))).

test(scaleAt) :-
	scaleAt([d, e, f], 5, Tone1), Tone1 == e,
	scaleAt([d, e, f], Index2, Tone2), Index2 == 3, Tone2 == f.

test(scaleToneF, [nondet]) :-
	scaleToneF(Scale1, (b, 2), 1), Scale1 == (c, major),
	scaleToneF(Scale2, (b, 2), 0), Scale2 == (d, minor),
	not(scaleToneF((c, major), (b, 2), 0)).

test(scaleChordF, [nondet]) :-
	scaleChordF(Scale1, [c, d, e], 1), Scale1 == (c, major),
	scaleChordF(Scale2, [c, d, eis, f], 0.75), Scale2 == (d, minor),
	not(scaleChordF((c, major), [d, bes], 1)).

test(scaleSongF, [setup(testSong), cleanup(clear)]) :-
	once((scaleSongF(Scale1, Fuzzy1), Scale1 == (d, minor))),
	Fuzzy1 < 0.9, Fuzzy1 > 0.8,
	once((scaleSongF(Scale2, Fuzzy2), Scale2 == (gis, minor))),
	Fuzzy2 < 0.1.

test(sortedSongScales, [setup(testSong), cleanup(clear)]) :-
	sortedSongScales(Scales),
	nth1(1, Scales, ((f, major), _)).

:- end_tests(scales).

