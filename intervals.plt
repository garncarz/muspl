:- begin_tests(intervals).

test(toneToIntervalToC1, [set(Tone == [tone{pitch:dis, octave:1},
		tone{pitch:es, octave:1}])]) :-
	toneToIntervalToC(Tone, 15).
test(toneToIntervalToC2, [set(Diff == [(-20, -1)])]) :-
	toneToIntervalToC(tone{pitch:es, octave:(-2)}, Diff).

test(intervalDiff) :-
	intervalDiff((2, 1), (2, 1), 0),
	intervalDiff(tone{pitch:d}, tone{pitch:g}, Diff1), Diff1 == 5,
	intervalDiff(tone{pitch:g}, tone{pitch:d}, Diff2), Diff2 == -5,
	intervalDiff(tone{pitch:ais, octave:1}, tone{pitch:b, octave:(-1)}, Diff3),
		Diff3 == -23,
	once((intervalDiff(Tone1, tone{pitch:b, octave:(-1)}, -10),
		tone{pitch:a, octave:0} :< Tone1)),
	once((intervalDiff(tone{pitch:g}, Tone2, 3), tone{pitch:ais} == Tone2)).

:- end_tests(intervals).

