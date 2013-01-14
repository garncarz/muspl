:- begin_tests(data).

test(sameStaff) :-
	sameStaff((4, 1, f), (6, 2, f)),
	not(sameStaff((4, 1, f), (6, 2, g))).

test(timeCmp1) :-
	timeCmp(D1, (3, 2), (4, 1)), D1 == '<',
	timeCmp(D2, (10, 3), (10, 3)), D2 == '=',
	timeCmp(D3, (3, 2), (2, 5)), D3 == '>'.
test(timeCmp2) :-
	timeCmp(D1, (3, 2, g), (4, 1, g)), D1 == '<',
	timeCmp(D2, (10, 3, g), (10, 3, g)), D2 == '=',
	timeCmp(D3, (3, 2, g), (2, 5, g)), D3 == '>'.
test(timeCmp2Fail, [fail]) :-
	timeCmp(_, (1, 1, g), (1, 1, f)).

:- end_tests(data).

