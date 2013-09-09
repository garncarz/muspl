#!/usr/bin/swipl --nopce -f

:- doc_server(4000, [allow(ip(127,0,0,1)), edit(false)]).
:- [data, dataMidi, theory, lilypond, ultrastar].
:- run_tests.

wieg :- wiegL, wiegW.
wiegL :- loadData('wiegenlied').
wiegW :- exportLy('wiegenlied.ly').

:- wiegL.

