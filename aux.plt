:- begin_tests(aux).

test(avg_list) :-
	avg_list([3, 6, -12], Avg), Avg == -1.

test(map_list) :-
	map_list(plus(2), 3, Mapped1), Mapped1 == [5],
	map_list(plus(2), [1, -1], Mapped2), Mapped2 == [3, 1].

:- end_tests(aux).

