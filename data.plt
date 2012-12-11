:- begin_tests(data).

:- use_module(data).

test(timeCmp) :-
	timeCmp('<', (3, 2, g), (4, 1, g)),
	timeCmp('=', (10, 3, g), (10, 3, g)),
	timeCmp('>', (3, 2, g), (2, 5, g)).
test(timeCmpFail, [fail]) :-
	timeCmp(_, (1, 1, g), (1, 1, f)).

:- end_tests(data).

