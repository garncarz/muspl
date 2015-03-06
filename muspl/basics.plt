:- begin_tests(basics).

test(toneName) :-
	toneName(d, Name1), Name1 == d,
	toneName((e, minor), Name2), Name2 == e.

test(toneFromChord) :-
	toneFromChord((fis, 1), [b, (fis, 0), d]),
	not(chordTone([b, (fis, 0), d], (g, 1))).

test(sameChords) :-
	sameChords([c, (e, 1), (g, -1)], [e, (g, 1), (c, 2)]),
	not(sameChords([c, (e, 1), (g, -1), f], [e, (g, 1), (c, 2)])).

:- end_tests(basics).

