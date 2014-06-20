:- module(construction, [
	% copying:
	copyBars/5, copyBars/3, copyBar/2, copyBarsCond/4,
	
	% conditions:
	isStaff/2, true/1,
	
	% transformations:
	pitchShift/3
	]).

:- use_module(data).
:- use_module(intervals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COPYING:

copyBars(Start, Dest, Len, Cond, Action) :-
	DestEnd is Dest + Len - 1,
	between(Dest, DestEnd, Bar),
	Bar1 is Bar - (Dest - Start),
	notation((Bar1, Beat, Staff), Pitch, Duration),
	call(Cond, ((Bar1, Beat, Staff), Pitch, Duration)),
	call(Action, ((Bar, Beat, Staff), Pitch, Duration),
		((Bar2, Beat2, Staff2), Pitch2, Duration2)),
	assertz(notation((Bar2, Beat2, Staff2), Pitch2, Duration2)),
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

isStaff(Staff, ((_, _, Staff), _, _)).
true(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORMATIONS:

pitchShift(Shift, ((Bar, Beat, Staff), Pitch1, Duration),
	((Bar, Beat, Staff), Pitch2, Duration)) :-
	once((intervalDiff(Pitch1, Pitch2, Shift);
		Pitch2 = Pitch1
		)).

