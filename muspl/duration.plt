:- begin_tests(duration, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(beats, [setup(ts34), cleanup(clear)]) :-
    duration{len:[]}.beats() == 0,
    duration{len:8}.beats() == 0.5,
    duration{len:[1, 2]}.beats() == 6.
test(beats2, [setup(ts68), cleanup(clear)]) :-
    duration{len:[]}.beats() == 0,
    duration{len:8}.beats() == 1,
    duration{len:[1, 2]}.beats() == 12.

test(fromBeats1, [setup(ts34), cleanup(clear)]) :-
    duration{}.fromBeats(2).len == [2],
    duration{}.fromBeats(4).len == [1],
    duration{}.fromBeats(2.5).len == [2, 8].
test(fromBeats2, [setup(ts68), cleanup(clear)]) :-
    duration{}.fromBeats(2).len == [4],
    duration{}.fromBeats(1).len == [8].

test(negBeats, [setup(ts34), cleanup(clear)]) :-
    duration{}.fromBeats(-2.5) = NegDur,
    NegDur.len == [2, 8],
    NegDur.get(negative),
    NegDur.beats() == -2.5.

test(norm) :-
    duration{len:1.6}.norm() == [2, 8],
    duration{len:4}.norm() == [4],
    duration{len:0}.norm() == [],
    duration{len:999}.norm() == [],
    duration{len:0.5}.norm() == [1, 1].

test(add) :-
    duration{len:8}.add(duration{len:[8]}).len == [4],
    duration{len:8}.add(duration{len:4}).len == [4, 8],
    duration{len:1}.add(duration{len:1}).len == [1, 1],
    duration{len:[8, 2]}.add(duration{len:8}).len == [2, 4].

test(cmp) :-
    duration{len:[4, 8]}.cmp(duration{len:2}) == '<',
    duration{len:[4, 8]}.cmp(duration{len:[8, 4]}) == '=',
    duration{len:[2, 2, 2]}.cmp(duration{len:1}) == '>',
    duration{len:4}.cmp(duration{len:0}) == '>',
    duration{len:[]}.cmp(duration{len:1}) == '<',
    duration{len:[]}.cmp(duration{len:0}) == '='.

:- end_tests(duration).
