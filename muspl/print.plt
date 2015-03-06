:- begin_tests(print).

test(stdChords) :-
	writeln('D major:'),
	stdChords((d, major)),
	
	writeln('G minor:'),
	stdChords((g, minor)).

:- end_tests(print).

