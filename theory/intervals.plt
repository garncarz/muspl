:- begin_tests(intervals).

test(toneToIntervalToC1, [set(Tone == [(dis, 1), (es, 1)])]) :-
	toneToIntervalToC(Tone, 15).
test(toneToIntervalToC2, [set(Diff == [(-20, -1)])]) :-
	toneToIntervalToC((es, -2), Diff).

test(intervalDiff) :-
	intervalDiff(cokolivStejneho, cokolivStejneho, 0),
	intervalDiff(d, g, Diff1), Diff1 == 5,
	intervalDiff(g, d, Diff2), Diff2 == -5.

:- end_tests(intervals).

