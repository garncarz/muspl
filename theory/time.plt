:- begin_tests(time, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

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

test(allSongChords, [setup(testSong), cleanup(clear)]) :-
	allSongChords(Chords),
	once((member(Chord1, Chords),
		Chord1 == [(f, 1), (d, 1), (c, 0), (bes, 0)])),
	not(member([(c, 1), (f, 3)], Chords)).

:- end_tests(time).

