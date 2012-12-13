:- begin_tests(theory, [setup(clear), cleanup(clear)]).

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


test(possibleFunc, [nondet]) :-
	possibleFunc(Func1), Func1 == 3,
	not(possibleFunc(12)).

test(harmonicFunc, [nondet]) :-
	harmonicFunc(Func, Members), Func == 6, Members == [6, 8, 10].

test(toneName) :-
	toneName(d, Name1), Name1 == d,
	toneName((e, minor), Name2), Name2 == e.

test(toneFromScale, [nondet]) :-
	toneFromScale(g, (c, major)),
	toneFromScale(bes, Scale), Scale == (d, minor),
	not(toneFromScale(eis, (c, major))).

test(toneFromChord) :-
	toneFromChord((fis, 1), [b, (fis, 0), d]),
	not(chordTone([b, (fis, 0), d], (g, 1))).

test(sameChords) :-
	sameChords([c, (e, 1), (g, -1)], [e, (g, 1), (c, 2)]),
	not(sameChords([c, (e, 1), (g, -1), f], [e, (g, 1), (c, 2)])).

test(chordFromScale, [nondet]) :-
	chordFromScale([e, (g, 1), (c, -1)], Scale), Scale == (d, minor),
	not(chordFromScale([eig, g], (c, major))).

test(scaleAt) :-
	scaleAt([d, e, f], 5, Tone1), Tone1 == e,
	scaleAt([d, e, f], Index2, Tone2), Index2 == 3, Tone2 == f.

% TODO doplnit not(...)
test(harmonicFuncChord, [nondet]) :-
	harmonicFuncChord(Scale1, Func1, Chord1),
	Scale1 == (fis, minor), Func1 == 3, Chord1 == [a, cis, e],
	harmonicFuncChord((a, major), Func2, [(fis, 2), (a, 0), d]),
	Func2 == 4.

test(scaleTone, [nondet]) :-
	scaleTone(Scale1, (b, 2), 1), Scale1 == (c, major),
	scaleTone(Scale2, (b, 2), 0), Scale2 == (d, minor),
	not(scaleTone((c, major), (b, 2), 0)).

test(scaleChord, [nondet]) :-
	scaleChord(Scale1, [c, d, e], 1), Scale1 == (c, major),
	scaleChord(Scale2, [c, d, eis, f], 0.75), Scale2 == (d, minor),
	not(scaleChord((c, major), [d, bes], 1)).

test(allSongChords, [setup(testSong), cleanup(clear)]) :-
	allSongChords(Chords),
	once((member(Chord1, Chords),
		Chord1 == [(f, 1), (d, 1), (c, 0), (bes, 0)])),
	not(member([(c, 1), (f, 3)], Chords)).

test(scaleSong, [setup(testSong), cleanup(clear)]) :-
	once((scaleSong(Scale1, Fuzzy1), Scale1 == (d, minor))),
	Fuzzy1 < 0.9, Fuzzy1 > 0.8,
	once((scaleSong(Scale2, Fuzzy2), Scale2 == (gis, minor))),
	Fuzzy2 < 0.1.

test(sortedSongScales, [setup(testSong), cleanup(clear)]) :-
	sortedSongScales(Scales),
	nth1(1, Scales, ((f, major), _)).

test(timeDiff1, [setup(ts34), cleanup(clear)]) :-
	timeDiff((10, 2), (9, 1), Diff), Diff == -4.
test(timeDiff2, [setup(ts68), cleanup(clear)]) :-
	timeDiff((10, 2), (9, 1), Diff2), Diff2 == -7.

test(durationToBeats1, [setup(ts34), cleanup(clear)]) :-
	durationToBeats([], Beats1), Beats1 == 0,
	durationToBeats(8, Beats2), Beats2 == 0.5,
	durationToBeats([1, 2], Beats3), Beats3 == 6.
test(durationToBeats2, [setup(ts68), cleanup(clear)]) :-
	durationToBeats([], Beats4), Beats4 == 0,
	durationToBeats(8, Beats5), Beats5 == 1,
	durationToBeats([1, 2], Beats6), Beats6 == 12.

test(toneAtTime1, [setup(testSong), cleanup(clear),
		set(Tone == [(d, 1), (bes, 1)])]) :-
	toneAtTime(Tone, (1, 2)).
test(toneAtTime2, [setup(testSong), cleanup(clear),
		all(Tone == [r])]) :-
	toneAtTime(Tone, (4, 3)).

test(chordAtTime, [setup(testSong), cleanup(clear)]) :-
	chordAtTime(Chord, (1, 3)),
	sort(Chord, ChordSorted), ChordSorted == [(a, 1), (c, 1), (f, 0), (f, 1)].

:- end_tests(theory).

