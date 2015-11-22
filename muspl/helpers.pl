:- module(helpers, [
	avg_list/2,
	inverse/2,
	map_list/3,
	multiAssert/1,
	verbose/1,
	writeTree/1
	]).

:- use_module(data).

:- ['helpers.plt'].

%% avg_list(+List, -Avg)
% Average value of List is Avg.
avg_list(List, Avg) :-
	length(List, Len), Len > 0,
	sum_list(List, Sum),
	Avg is Sum / Len.

%% inverse(+Number, -Inversed)
% Inversed = 1 / Number
inverse(0, 0) :- !.
inverse(X, I) :-
	I is 1 / X.

%% map_list(:Goal, +List, -MappedList).
%% map_list(:Goal, +Item, -MappedItem).
% Extension of maplist/3, can map on an atom returning a list with the result.
map_list(Goal, List, Mapped) :-
	is_list(List), !,
	maplist(Goal, List, Mapped).
map_list(Goal, Item, [Mapped]) :-
	call(Goal, Item, Mapped).

%% multiAssert(+Facts)
% Assertzs all the Facts from a list.
multiAssert([Fact | Rest]) :-
	assertz(Fact),
	multiAssert(Rest).
multiAssert([]).

verbose(Info) :-
	write(user_output, Info),
	nl(user_output).

writeTree(Tree) :-
	writeTree(0, Tree), !.
writeTree(Level, X) :-
	writeIndent(Level),
	ourAtom(X) -> writeln(X);
		(X =.. [',' | Content] -> writeln('(');
			is_list(X) -> Content = X, writeln('[')),
		writeTreeList(Level + 1, Content).
writeTreeList(_, []).
writeTreeList(Level, [First | Rest]) :-
	writeTree(Level, First),
	writeTreeList(Level, Rest).
writeIndent(Level) :-
	Level > 0,
	write('  '),
	writeIndent(Level - 1).
writeIndent(_).

ourAtom(Atom) :-
	atom(Atom);
	number(Atom);
	Atom = (_, _);
	Atom =.. [notation | _].

