:- begin_tests(position, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(timeDiff1, [setup(ts34), cleanup(clear)]) :-
	position{bar:10, beat:2}.diff(position{bar:9, beat:1}).beats() == -4,
	position{bar:9, beat:1}.diff(position{bar:9, beat:1}).beats() == 0.
test(timeDiff2, [setup(ts68), cleanup(clear)]) :-
	position{bar:9, beat:1}.diff(position{bar:10, beat:2}).beats() == 7,
	position{bar:2, beat:2}.diff(position{bar:4, beat:1}).beats() == 11.

:- end_tests(position).

