:- begin_tests(musicTime, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(simpleTime) :-
	simpleTime((2, 3, f), Simple1), Simple1 == (2, 3),
	simpleTime((4, 1), Simple2), Simple2 == (4, 1).

test(timeDiff1, [setup(ts34), cleanup(clear)]) :-
	timeDiff((10, 2), (9, 1), Diff), Diff == -4,
	timeDiff((9, 1), (9, 1), Diff2), Diff2 == 0.
test(timeDiff2, [setup(ts68), cleanup(clear)]) :-
	timeDiff((9, 1), (10, 2), Diff), Diff == 7,
	timeDiff((2, 2), (4, 1), Diff2), Diff2 == 11.

test(durationToBeats1, [setup(ts34), cleanup(clear)]) :-
	durationToBeats([], Beats1), Beats1 == 0,
	durationToBeats(8, Beats2), Beats2 == 0.5,
	durationToBeats([1, 2], Beats3), Beats3 == 6.
test(durationToBeats2, [setup(ts68), cleanup(clear)]) :-
	durationToBeats([], Beats1), Beats1 == 0,
	durationToBeats(8, Beats2), Beats2 == 1,
	durationToBeats([1, 2], Beats3), Beats3 == 12.

test(beatsToDuration1, [setup(ts34), cleanup(clear)]) :-
	beatsToDuration(2, Dur1), Dur1 == [2],
	beatsToDuration(4, Dur2), Dur2 == [1],
	beatsToDuration(2.5, Dur3), Dur3 == [2, 8].
test(beatsToDuration2, [setup(ts68), cleanup(clear)]) :-
	beatsToDuration(2, Dur1), Dur1 == [4],
	beatsToDuration(1, Dur2), Dur2 == [8].

test(normalizedDuration) :-
	normalizedDuration(1.6, Dur1), Dur1 == [2, 8],
	normalizedDuration(4, Dur2), Dur2 == [4],
	normalizedDuration(0, Dur3), Dur3 == [],
	normalizedDuration(999, Dur4), Dur4 == [],
	normalizedDuration(0.5, Dur5), Dur5 == [1, 1].

test(addDurations) :-
	addDurations(8, [8], Dur1), Dur1 == [4],
	addDurations(8, 4, Dur2), Dur2 == [4, 8],
	addDurations(1, 1, Dur3), Dur3 == [1, 1],
	addDurations([8, 2], 8, Dur4), Dur4 == [2, 4].

test(dursInvCmp) :-
	dursInvCmp(Delta1, [4, 8], 2), Delta1 == '>',
	dursInvCmp(Delta2, [4, 8], [8, 4]), Delta2 == '=',
	dursInvCmp(Delta3, [2, 2, 2], 1), Delta3 == '<'.

test(toneAtTime1, [setup(testSong), cleanup(clear),
		set(Tone == [(d, 1), (bes, 1)])]) :-
	toneAtTime(Tone, (1, 2)).
test(toneAtTime2, [setup(testSong), cleanup(clear),
		all(Tone == [r])]) :-
	toneAtTime(Tone, (4, 3)).

test(chordAtTime, [setup(testSong), cleanup(clear)]) :-
	chordAtTime(Chord, (1, 3)),
	sort(Chord, ChordSorted), ChordSorted == [(a, 1), (c, 1), (f, 0), (f, 1)].

test(allSongChords, [setup(testSong), cleanup(clear)]) :-
	allSongChords(Chords),
	once((member(Chord1, Chords),
		Chord1 == [(f, 1), (d, 1), (c, 0), (bes, 0)])),
	not(member([(c, 1), (f, 3)], Chords)).

:- end_tests(musicTime).

