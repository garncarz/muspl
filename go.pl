#!/usr/bin/swipl --nopce -f

:- doc_server(4000, [edit(false)]).
:- [data, theory, lilypond].
:- run_tests.

wieg :- loadData('wiegenlied').

:- wieg.

