:- module(aux, [
	avg_list/2,
	multiAssert/1
	]).

:- use_module(data).

avg_list(List, Avg) :-
	length(List, Len), Len > 0,
	sum_list(List, Sum),
	Avg is Sum / Len.

multiAssert([Fact | Rest]) :-
	assertz(Fact),
	multiAssert(Rest).
multiAssert([]).

