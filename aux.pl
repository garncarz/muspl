:- module(aux, [
	avg_list/2,
	inverse/2,
	map_list/3,
	multiAssert/1
	]).

:- use_module(data).

:- ['aux.plt'].

avg_list(List, Avg) :-
	length(List, Len), Len > 0,
	sum_list(List, Sum),
	Avg is Sum / Len.

inverse(X, I) :-
	I is 1 / X.

map_list(Action, List, Mapped) :-
	is_list(List), !,
	maplist(Action, List, Mapped).
map_list(Action, X, [Mapped]) :-
	call(Action, X, Mapped).

multiAssert([Fact | Rest]) :-
	assertz(Fact),
	multiAssert(Rest).
multiAssert([]).

