:- module(harmony, [
	harmonicFuncSymb/2,
	harmonicFuncChord/3
	]).

:- use_module(basics).
:- use_module(scales).

:- ['harmony.plt'].

%% harmonicFuncSymb(-Func, -Symbol)
harmonicFuncSymb(1, 'T').
harmonicFuncSymb(2, 'II').
harmonicFuncSymb(3, 'III').
harmonicFuncSymb(4, 'S').
harmonicFuncSymb(5, 'D').
harmonicFuncSymb(6, 'VI').
harmonicFuncSymb(7, 'VII').

%% possibleFunc(-Func)
% Func is a possible harmonic function.
possibleFunc(Func) :- harmonicFuncSymb(Func, _).

%% harmonicFunc(-Func, -Members)
% Harmonic function Func has tones from scale relatively determined by Members.
% E.g. =|harmonicFunc(1, [1, 3, 5]).|=
harmonicFunc(Func, Members) :-
	once((number(Func); var(Func))),
	possibleFunc(Func),
	maplist(plus(Func), [0, 2, 4], Members).

%% harmonicFuncChord(-Scale, -Func, -Chord)
% Chord is of Scale's harmonic function Func.
%
% @tbd Func is 0 when unknown, 'd better fail.
harmonicFuncChord(Scale, Func, Chord) :- var(Chord),
	scale(Scale, ScaleTones),
	harmonicFunc(Func, FuncMembers),
	maplist(scaleAt(ScaleTones), FuncMembers, Chord).
harmonicFuncChord(Scale, Func, Chord) :- is_list(Chord),
	scale(Scale, ScaleTones),
	harmonicFunc(Func, FuncMembers),
	maplist(scaleAt(ScaleTones), FuncMembers, FuncChord),
	sameChords(FuncChord, Chord).
harmonicFuncChord(_, 0, _).

