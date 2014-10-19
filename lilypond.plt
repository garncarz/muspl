:- begin_tests(lilypond, [setup(clear), cleanup(clear)]).

:- ['testSong.plt'].

test(pitchLily) :-
	pitchLily(tone{pitch:des, octave:2}, L1), L1 == 'des\'\'',
	pitchLily(tone{pitch:cis, octave:0}, L2), L2 == 'cis',
	pitchLily(tone{pitch:a, octave:(-1)}, L3), L3 == 'a,'.

test(chordLily) :-
	chordLily((_, [tone{pitch:a, octave:1}, tone{pitch:f, octave:1}], 8), Ch1),
		Ch1 == '<a\' f\'>8',
	chordLily((_, [tone{pitch:a, octave:1}], 4), Ch2), Ch2 == 'a\'4',
	chordLily((_, [tone{pitch:a, octave:1}, tone{pitch:f, octave:1}], [8, 4]),
		Ch3), Ch3 == '<a\' f\'>8 ~<a\' f\'>4'.

test(restLily) :-
	restLily((_, r, 2), R1), R1 == 'r2',
	restLily((_, [r], 4), R2), R2 == 'r4',
	restLily((_, s, [1, 8]), S1), S1 == 's1 s8'.

test(conflictChords, [setup(ts34), cleanup(clear)]) :-
	conflictChords((time{bar:2, beat:1, staff:g}, _, 2),
		(time{bar:2, beat:2, staff:g}, _, 8)),
	conflictChords((time{bar:3, beat:1, staff:f}, _, 4),
		(time{bar:3, beat:1, staff:f}, _, 8)),
	not(conflictChords((time{bar:3, beat:1, staff:f}, _, 4),
		(time{bar:3, beat:1, staff:g}, _, 8))),
	not(conflictChords((time{bar:3, beat:1, staff:g}, _, 2),
		(time{bar:4, beat:1, staff:g}, _, 8))),
	conflictChords((time{bar:3, beat:1, staff:g}, _, 1),
		(time{bar:4, beat:1, staff:g}, _, 8)),
	conflictChords((time{bar:1, beat:1, staff:g}, _, [8, 8, 8]),
		(time{bar:1, beat:1.5, staff:g}, _, 4)).

test(spaceFiller, [setup(ts68), cleanup(clear)]) :-
	spaceFiller(time{bar:2, beat:2}, time{bar:4, beat:1}, Filler),
		Filler =@= (_, s, [1, 4, 8]).

:- end_tests(lilypond).

