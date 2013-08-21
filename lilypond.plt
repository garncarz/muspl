:- begin_tests(lilypond, [setup(clear), cleanup(clear)]).

:- ['testSong.plt'].

test(pitchLily) :-
	pitchLily((des, 2), L1), L1 == 'des\'\'',
	pitchLily((cis, 0), L2), L2 == 'cis',
	pitchLily((a, -1), L3), L3 == 'a,'.

test(chordLily) :-
	chordLily((_, [(a, 1), (f, 1)], 8), Ch1), Ch1 == '<a\' f\'>8',
	chordLily((_, [(a, 1)], 4), Ch2), Ch2 == 'a\'4',
	chordLily((_, [(a, 1), (f, 1)], [8, 4]), Ch3),
		Ch3 == '<a\' f\'>8 ~<a\' f\'>4'.

test(restLily) :-
	restLily((_, r, 2), R1), R1 == 'r2',
	restLily((_, [r], 4), R2), R2 == 'r4',
	restLily((_, s, [1, 8]), S1), S1 == 's1 s8'.

test(conflictChords, [setup(ts34), cleanup(clear)]) :-
	conflictChords(((2, 1, g), _, 2), ((2, 2, g), _, 8)),
	conflictChords(((3, 1, f), _, 4), ((3, 1, f), _, 8)),
	not(conflictChords(((3, 1, f), _, 4), ((3, 1, g), _, 8))),
	not(conflictChords(((3, 1, g), _, 2), ((4, 1, g), _, 8))),
	conflictChords(((3, 1, g), _, 1), ((4, 1, g), _, 8)),
	conflictChords(((1, 1, g), _, [8, 8, 8]), ((1, 1.5, g), _, 4)).

test(spaceFiller, [setup(ts68), cleanup(clear)]) :-
	spaceFiller((2, 2), (4, 1), Filler), Filler =@= (_, s, [1, 4, 8]).

:- end_tests(lilypond).

