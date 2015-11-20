:- begin_tests(position, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(diff1, [setup(ts34), cleanup(clear)]) :-
    position{bar:10, beat:2}.diff(position{bar:9, beat:1}).beats() == -4,
    position{bar:9, beat:1}.diff(position{bar:9, beat:1}).beats() == 0.
test(diff2, [setup(ts68), cleanup(clear)]) :-
    position{bar:9, beat:1}.diff(position{bar:10, beat:2}).beats() == 7,
    position{bar:2, beat:2}.diff(position{bar:4, beat:1}).beats() == 11.

test(cmp) :-
    position{bar:5, beat:1}.cmp(position{bar:4, beat:3}) = '>',
    position{bar:2, beat:1}.cmp(position{bar:4, beat:3}) = '<',
    position{bar:5, beat:1}.cmp(position{bar:5, beat:1}) = '='.

test(elapsed1, [setup(ts34), cleanup(clear)]) :-
    position{bar:1, beat:1}.elapsed().beats() == 0,
    position{bar:3, beat:2}.elapsed().beats() == 7.
test(elapsed2, [setup(ts68), cleanup(clear)]) :-
    position{bar:1, beat:1}.elapsed().beats() == 0,
    position{bar:3, beat:2}.elapsed().beats() == 13.

test(addDiffCmp1, [setup(ts34), cleanup(clear)]) :-
    P1 = position{bar:4, beat:2},
    P2 = position{bar:7, beat:3},
    P1.add(P1.diff(P2)).cmp(P2) = '='.
test(addDiffCmp2, [setup(ts68), cleanup(clear)]) :-
    P1 = position{bar:4, beat:2},
    P2 = position{bar:7, beat:3},
    P1.add(P1.diff(P2)).cmp(P2) = '='.

:- end_tests(position).
