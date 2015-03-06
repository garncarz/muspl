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
:- use_module(musicTime).

process :-
	retractall(notation(_, _, _)),
	fail.
process :-
	retract(♪ Action),
	process(Action),
	fail.
process.
process(Action) :-
	melody{} :< Action,
	member(Key, [pitch, len]),
	get_dict(Key, Action, Value),
	Value ♪= Eval,
	ActionEval = Action.put(Key, Eval),
	process(ActionEval), !.
process(Action) :-
	melody{start:(Bar, Beat, Staff), relative:(Pitch, Octave, Dur1),
		pitch:[PitchDiff | RestPitchDiff]} :< Action,
	(_{len:[Len | RestLen]} :< Action; Len = 1, RestLen = []),
	(is_dict(Len, exact), exact{dur:Dur} :< Len; durMul(Dur1, Len, Dur)),
	Time = time{bar: Bar, beat:Beat, staff:Staff},
	(PitchDiff \= r ->
		extra Scale, is_dict(Scale, scale),
		% TODO Tone = Scale.add(tone{pitch: Pitch, octave:Octave}, PitchDiff),
		PitchShift = Scale.intAtFrom(PitchDiff, Pitch),
		Tone = tone{pitch: Pitch, octave:Octave}.add(PitchShift),
		assertz(notation(Time, Tone, Dur));
		true
	),
	timeAdd(Time, Dur, Time2), !,
	ActionRest = Action
		.put(start, (Time2.bar, Time2.beat, Time2.staff))
		.put(pitch, RestPitchDiff)
		.put(len, RestLen),
	process(ActionRest).
process(Action) :-
	copyBars{from:Bar1, to:Bar2} :< Action,
	(copyBars{count:Count} :< Action; Count = 1),
	(copyBars{cond:Cond} :< Action; Cond = true),
	(copyBars{action:Subaction} :< Action; Subaction = =),
	copyBars(Bar1, Bar2, Count, Cond, Subaction), !.
process(Action) :-
	del{cond:Cond} :< Action,
	notation(Time, Tone, Dur),
	call(Cond, (Time, Tone, Dur)),
	retract(notation(Time, Tone, Dur)),
	fail.
process(_).  % TODO maybe error

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COPYING:

copyBars(Start, Dest, Len, Cond, Action) :-
	DestEnd is Dest + Len - 1,
	between(Dest, DestEnd, Bar),
	Bar1 is Bar - (Dest - Start),
	notation(Time1, Pitch, Duration),
	Time1.bar = Bar1,
	Time21 = Time1.put(bar, Bar),
	call(Cond, (Time1, Pitch, Duration)),
	call(Action, (Time21, Pitch, Duration), (Time2, Pitch2, Duration2)),
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

