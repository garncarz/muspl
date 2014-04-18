:- module(print, [
	stdChords/1
	]).

:- use_module(harmony).
:- use_module(symbolChords).

%% stdChords(+Scale)
% Prints standard chords from the Scale.
stdChords(Scale) :-
	harmonicFuncChord(Scale, Func, ChordTones),
	once((symbolChord(Chord, ChordTones))),
	writeln(Func:Chord),
	fail; true.

