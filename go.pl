#!/usr/bin/swipl --nopce -f

:- doc_server(4000, [edit(false)]).
:- [data, dataMidi, theory, lilypond].
:- run_tests.

wieg :- wiegL, wiegW.
wiegL :- loadData('wiegenlied').
wiegW :- export('wiegenlied.ly').

:- wiegL.

