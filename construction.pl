:- module(construction, [
	process/0,

	% copying:
	copyBars/5, copyBars/3, copyBar/2, copyBarsCond/4,
	
	% conditions:
	isStaff/2, true/1,
	
	% transformations:
	pitchShift/3
	]).

:- use_module(data).

process :-
	retractall(notation(_, _, _)),
	fail.
process :-
	retract(♪ Action),
	process(Action),
	fail.
process.
process(Action) :-
	melody{start:(Bar, Beat, Staff), relative:(Pitch, Octave, Dur1),
		run:[Actual | Rest]} :< Action,
	(Actual = (ScaleDiff, Dur); Actual = ScaleDiff, Dur = Dur1),
	Time = time{bar: Bar, beat:Beat, staff:Staff},
	(Actual \= r ->
		extra Scale, is_dict(Scale, scale),
		PitchShift = Scale.intAtFrom(ScaleDiff, Pitch),
		Tone = tone{pitch: Pitch, octave:Octave}.add(PitchShift),
		assertz(notation(Time, Tone, Dur));
		true
	),
	timeAdd(Time, Dur, Time2), !,
	process(melody{start:(Time2.bar, Time2.beat, Time2.staff),
		relative:(Pitch, Octave, Dur1), run:Rest}).
process(Action) :-
	copyBars{from:Bar1, to:Bar2, cond:Cond} :< Action,
	copyBarsCond(Bar1, Bar2, 1, Cond).
process(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COPYING:

copyBars(Start, Dest, Len, Cond, Action) :-
	DestEnd is Dest + Len - 1,
	between(Dest, DestEnd, Bar),
	Bar1 is Bar - (Dest - Start),
	notation(Time1, Pitch, Duration),
	Time1.bar = Bar1,
	call(Cond, (Time1, Pitch, Duration)),
	call(Action, (Time1, Pitch, Duration), (Time2, Pitch2, Duration2)),
	assertz(notation(Time2, Pitch2, Duration2)),
	fail.
copyBars(_, _, _, _, _).

copyBars(Start, Dest, Len) :-
	copyBars(Start, Dest, Len, true, =).

copyBar(Start, Dest) :-
	copyBars(Start, Dest, 1).

copyBarsCond(Start, Dest, Len, Cond) :-
	copyBars(Start, Dest, Len, Cond, =).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONDITIONS:

isStaff(Staff, (Time1, _, _)) :- Time1.staff = Staff.
true(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORMATIONS:

pitchShift(Shift, (Time, Tone1, Duration), (Time, Tone2, Duration)) :-
	once((Tone2 = Tone1.add(Shift); Tone2 = Tone1)).

