:- module(print, [
	scales/0,
	stdChords/1
	]).

:- use_module(scales).
:- use_module(harmony).
:- use_module(symbolChords).

scales :-
	scale(Scale, _),
	format('~w~n', [Scale]),
	fail; true.

%% stdChords(+Scale)
% Prints standard chords from the Scale.
stdChords(Scale) :-
	harmonicFuncChord(Scale, Func, ChordTones),
	harmonicFuncSymb(Func, Symb),
	once((symbolChord(Chord, ChordTones))),
	format('~t~w~4| ~w~n', [Symb, Chord]),
	fail; true.

