:- begin_tests(dataMidi).

test(toneFromMidi, [nondet]) :-
	toneFromMidi(65, Tone1), Tone1 == (f, 0),
	toneFromMidi(36, Tone2), Tone2 == (c, -2),
	toneFromMidi(106, Tone3), Tone3 == (ais, 3).

:- end_tests(dataMidi).

