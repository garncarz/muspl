:- use_module(helpers).
:- use_module(data).

clear :- clearData.

ts34 :- assertz(extra timeSignature(3, 4)).

ts68 :- assertz(extra timeSignature(6, 8)).

easyNot((Bar, Beat, Staff), (Pitch, Octave), Dur) :-
    assertz(notation(position{bar:Bar, beat:Beat, staff:Staff},
        tone{pitch:Pitch, octave:Octave}, duration{len:Dur})).
easyNot((Bar, Beat, Staff), Note, Dur) :-
    assertz(notation(position{bar:Bar, beat:Beat, staff:Staff},
        Note, duration{len:Dur})).

testSong :-
    assertz(extra timeSignature(6, 8)),

    easyNot((1, 1, g), (a, 1), 8),
    easyNot((1, 1, g), (f, 1), 8),
    easyNot((1, 2, g), (bes, 1), 8),
    easyNot((1, 3, g), (a, 1), 8),
    easyNot((1, 3, g), (f, 1), 8),
    easyNot((1, 4, g), (g, 1), 8),
    easyNot((1, 4, g), (e, 1), 8),
    easyNot((1, 5, g), (f, 1), 8),
    easyNot((1, 5, g), (d, 1), 8),
    easyNot((1, 6, g), (g, 1), 8),
    easyNot((1, 6, g), (e, 1), 8),

    easyNot((1, 1, f), (f, 0), 8),
    easyNot((1, 1, f), (c, 1), 8),
    easyNot((1, 2, f), (d, 1), 8),
    easyNot((1, 3, f), (f, 0), 8),
    easyNot((1, 3, f), (c, 1), 8),
    easyNot((1, 4, f), (c, 0), 4),
    easyNot((1, 4, f), (bes, 0), 4),
    easyNot((1, 6, f), (c, 0), 8),
    easyNot((1, 6, f), (bes, 0), 8),

    easyNot((2, 1, g), r, 8), !.
