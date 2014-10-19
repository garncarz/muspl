:- begin_tests(tone).

test(toneToIntervalToC1, [set(Tone == [tone{pitch:dis, octave:1},
		tone{pitch:es, octave:1}])]) :-
	toneToIntervalToC(Tone, 15).
test(toneToIntervalToC2, [set(Diff == [(-20, -1)])]) :-
	toneToIntervalToC(tone{pitch:es, octave:(-2)}, Diff).

test(intervalDiff) :-
	tone{pitch:d}.diff(tone{pitch:g}) == 5,
	tone{pitch:g}.diff(tone{pitch:d}) == -5,
	tone{pitch:ais, octave:1}.diff(tone{pitch:b, octave:(-1)}) == -23,
	once((tone{pitch:b, octave:(-1)} :< tone{pitch:a, octave:0}.add(-10))),
	once((tone{pitch:g}.add(3) == tone{pitch:ais})).

:- end_tests(tone).

