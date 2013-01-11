:- begin_tests(harmony).

test(possibleFunc, [nondet]) :-
	possibleFunc(Func1), Func1 == 3,
	not(possibleFunc(12)).

test(harmonicFunc, [nondet]) :-
	harmonicFunc(Func, Members), Func == 6, Members == [6, 8, 10].

% TODO doplnit not(...)
test(harmonicFuncChord, [nondet]) :-
	harmonicFuncChord(Scale1, Func1, Chord1),
	Scale1 == (fis, minor), Func1 == 3, Chord1 == [a, cis, e],
	harmonicFuncChord((a, major), Func2, [(fis, 2), (a, 0), d]),
	Func2 == 4.

:- end_tests(harmony).

