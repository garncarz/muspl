:- module(tone, []).

:- ['tone.plt'].

dbToneToIntervalToC(c, (0, 0)).
dbToneToIntervalToC(ces, (0, -1)).
dbToneToIntervalToC(cis, (0, 1)).

dbToneToIntervalToC(d, (2, 0)).
dbToneToIntervalToC(des, (2, -1)).
dbToneToIntervalToC(dis, (2, 1)).

dbToneToIntervalToC(e, (4, 0)).
dbToneToIntervalToC(es, (4, -1)).
dbToneToIntervalToC(eis, (4, 1)).

dbToneToIntervalToC(f, (5, 0)).
dbToneToIntervalToC(fes, (5, -1)).
dbToneToIntervalToC(fis, (5, 1)).

dbToneToIntervalToC(g, (7, 0)).
dbToneToIntervalToC(ges, (7, -1)).
dbToneToIntervalToC(gis, (7, 1)).

dbToneToIntervalToC(a, (9, 0)).
dbToneToIntervalToC(as, (9, -1)).
dbToneToIntervalToC(ais, (9, 1)).

dbToneToIntervalToC(b, (11, 0)).
dbToneToIntervalToC(bes, (11, -1)).
dbToneToIntervalToC(bis, (11, 1)).


toneToIntervalToC(Pitch, (Base, Detail)) :-
    nonvar(Pitch),
    dbToneToIntervalToC(Pitch, (Base, Detail)).
% TODO remove
%toneToIntervalToC(tone{pitch:Pitch}, (Base, Detail)) :-
%   dbToneToIntervalToC(Pitch, (Base, Detail)).
toneToIntervalToC(Tone, (Base, Detail)) :-
    is_dict(Tone, tone), Tone :< tone{pitch: Pitch},
    dbToneToIntervalToC(Pitch, (Base, Detail)), !.
toneToIntervalToC(Tone, (Base, Detail)) :-
    is_dict(Tone, tone), integer(Tone.octave),
    dbToneToIntervalToC(Tone.pitch, (Base1, Detail)), !,
    Base is Tone.octave * 12 + Base1.
toneToIntervalToC(Tone, Int) :-
    integer(Int),
    Int1 is Int mod 12,
    (dbToneToIntervalToC(Pitch, (Int1, 0));
        Int2 is Int1 - 1, dbToneToIntervalToC(Pitch, (Int2, 1));
        Int2 is Int1 + 1, dbToneToIntervalToC(Pitch, (Int2, -1))),
    Octave is Int div 12,
    Tone = tone{pitch:Pitch, octave:Octave}.
toneToIntervalToC(Tone, (Base, Detail)) :-
    integer(Base), integer(Detail),
    Base1 is Base mod 12,
    dbToneToIntervalToC(Pitch, (Base1, Detail)),
    Octave is Base div 12,
    Tone = tone{pitch:Pitch, octave:Octave}.

intervalDiff((Base1, Detail1), (Base2, Detail2), Diff) :-
    integer(Base1), integer(Detail1), integer(Base2), integer(Detail2), !,
    Diff is (Base2 + Detail2) - (Base1 + Detail1).
intervalDiff((Base1, Detail1), (Base2, Detail2), Diff) :-
    integer(Base1), integer(Detail1), integer(Diff), !,
    (Detail2 = 0; Detail2 = -1; Detail2 = 1),
    Base2 is Base1 + Detail1 + Diff - Detail2.

Tone1.diff(Tone2) := Diff :-
    toneToIntervalToC(Tone1, Int1),
    toneToIntervalToC(Tone2, Int2), !,
    intervalDiff(Int1, Int2, Diff).

Tone1.add(Diff:Adjust) := Tone3 :-
    Tone2 = Tone1.add(Diff),
    dbToneToIntervalToC(Tone2.pitch, (Int, 0)),
    dbToneToIntervalToC(Pitch3, (Int, Adjust)),
    Tone3 = Tone2.put(pitch, Pitch3).
Tone1.add(Diff:Adjust) := Tone1.add(Diff).add(Adjust).
Tone1.add(Diff) := Tone2 :-
    toneToIntervalToC(Tone1, Int1),
    intervalDiff(Int1, Int2, Diff),
    toneToIntervalToC(Tone2A, Int2),
    (not(tone{octave:_} :< Tone1) -> del_dict(octave, Tone2A, _, Tone2);
        Tone2 = Tone2A).

Tone1.modDiff(Tone2, Diff) := true :-
    number(Diff),
    Diff mod 12 =:= Tone1.diff(Tone2) mod 12.
Tone1.modDiff(Tone2) := Diff :-
    Diff is Tone1.diff(Tone2) mod 12.

Tone.lowerOctave() := Tone.put(octave, Octave2) :-
    Octave2 is Tone.octave - 1.
Tone.higherOctave() := Tone.put(octave, Octave2) :-
    Octave2 is Tone.octave + 1.

Tone.pitchFreq() := Freq :-
    Diff = tone{pitch:a, octave:0}.diff(Tone),
    Freq is 440 * 2 ^ (Diff / 12).
