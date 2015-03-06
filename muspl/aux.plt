:- begin_tests(aux).

:- dynamic multiAssertTestPred/1.

test(avg_list) :-
	avg_list([3, 6, -12], Avg), Avg == -1.

test(inverse) :-
	inverse(10, Inv1), Inv1 =:= 0.1,
	inverse(0.2, Inv2), Inv2 =:= 5.

test(map_list) :-
	map_list(plus(2), 3, Mapped1), Mapped1 == [5],
	map_list(plus(2), [1, -1], Mapped2), Mapped2 == [3, 1].

test(multiAssert, [cleanup(retractall(multiAssertTestPred(_)))]) :-
	multiAssert([multiAssertTestPred(3), multiAssertTestPred(yes)]),
	once(aux:multiAssertTestPred(3)),
	once(aux:multiAssertTestPred(yes)),
	not(aux:multiAssertTestPred(no)).

:- end_tests(aux).

