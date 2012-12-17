:- begin_tests(aux).

test(avg_list) :-
	avg_list([3, 6, -12], Avg), Avg == -1.

:- end_tests(aux).

