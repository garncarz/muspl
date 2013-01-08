:- begin_tests(intervals).

test(toneToIntervalToC1, [set(Tone == [(dis, 1), (es, 1)])]) :-
	toneToIntervalToC(Tone, 15).
test(toneToIntervalToC2, [set(Diff == [(-20, -1)])]) :-
	toneToIntervalToC((es, -2), Diff).

test(intervalDiff) :-
	intervalDiff(cokolivStejneho, cokolivStejneho, 0),
	intervalDiff(d, g, Diff1), Diff1 == 5,
	intervalDiff(g, d, Diff2), Diff2 == -5,
	intervalDiff((ais, 1), (b, -1), Diff3), Diff3 == -23,
	once((intervalDiff(Tone1, (b, -1), -10), Tone1 == (a, 0))).

:- end_tests(intervals).

