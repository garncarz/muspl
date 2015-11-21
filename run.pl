#!/usr/bin/swipl --nopce -f

doc :- doc_server(4000, [allow(ip(127,0,0,1)), edit(false)]).

:- [muspl].
:- run_tests.

wieg :- wiegL, wiegW.
wiegL :- loadData('examples/wiegenlied').
wiegW :- exportLy('wiegenlied.ly').

:- wiegL.

