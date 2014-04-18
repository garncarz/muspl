:- module(print, [
	stdChords/1
	]).

:- use_module(harmony).
:- use_module(symbolChords).

%% stdChords(+Scale)
% Prints standard chords from the Scale.
stdChords(Scale) :-
	harmonicFuncChord(Scale, Func, ChordTones),
	harmonicFuncSymb(Func, Symb),
	once((symbolChord(Chord, ChordTones))),
	format('~t~w~4| ~w~n', [Symb, Chord]),
	fail; true.

