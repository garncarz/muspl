:- begin_tests(data).

:- use_module(data).

test(posCmp) :-
	posCmp('<', (3, 2, g), (4, 1, g)),
	posCmp('=', (10, 3, g), (10, 3, g)),
	posCmp('>', (3, 2, g), (2, 5, g)).
test(posCmpFail, [fail]) :-
	posCmp(_, (1, 1, g), (1, 1, f)).

:- end_tests(data).

