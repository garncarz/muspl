:- begin_tests(renderer).

test(addSamples) :-
    addSamples([0, 0, 2, 0, 1],
            1,    [2,-1],
               [0, 2, 1, 0, 1]).
test(sumSamples) :-
    sumSamples([0, 0, 0, 0, 0], [
           (1,    [1, 3]),
           (0, [-1,0,-1, 2, 1]),
           (2,     [0.5, 1, 2])
           ],  [-1,1,2.5,3, 3]).

:- end_tests(renderer).
