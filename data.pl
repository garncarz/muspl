:- module(data, [
	notation/3,
	timeSignature/2,
	notationScale/1,
	tempo/1,
	
	loadData/1,
	saveData/1,
	clearData,
	
	sameStaff/2,
	
	retractRedundant,
	
	allBeats/2,
	allBeats/1,
	
	timeCmp/3,
	
	sameSongs/2,
	songsDiff/4
	]).

:- dynamic
	notation/3,
	timeSignature/2,
	notationScale/1,
	tempo/1,
	
	notationDb/4.

:- discontiguous
	notation/3.

:- use_module(aux).

:- ['data.plt'].

loadData(Name) :-
	clearData,
	atomic_list_concat(['data/', Name, '.pl'], Filename),
	consult(Filename).

saveData(Name) :-
	atomic_list_concat(['data/', Name, '.pl'], Filename),
	tell(Filename),
	listing(notation/3),
	told.

clearData :-
	retractall(notation(_, _, _)),
	retractall(timeSignature(_, _)),
	retractall(notationScale(_)),
	retractall(tempo(_)).

sameStaff((_, _, Staff), (_, _, Staff)).
isStaff(Staff, ((_, _, Staff), _, _)).
true(_).

retractRedundant :-
	findall(notation(Time, Tone, Dur), notation(Time, Tone, Dur), Notation),
	sort(Notation, NotationSorted),
	retractall(notation(_, _, _)),
	multiAssert(NotationSorted).

%% allBeats(-Beats).
%% allBeats(+Staff, -Beats).
% True if notation/Staff contains music elements at Beats (sorted).
%
% @tbd All Beats (including exceeded) should be included.
allBeats(Beats) :-
	findall((Bar, Beat), notation((Bar, Beat, _), _, _), Starts),
	predsort(timeCmp, Starts, Beats).
allBeats(Staff, Beats) :-
	findall((Bar, Beat, Staff), notation((Bar, Beat, Staff), _, _), Starts),
	predsort(timeCmp, Starts, Beats).

%% timeCmp(-Delta, +Time1, +Time2)
% True if Time1 compared to Time2 is Delta.
timeCmp(Delta, (Bar1, Beat1, Staff), (Bar2, Beat2, Staff)) :-
	timeCmp(Delta, (Bar1, Beat1), (Bar2, Beat2)), !.
timeCmp(Delta, (Bar1, Beat1), (Bar2, Beat2)) :- number(Beat1), number(Beat2),
	(compare(Delta, Bar1, Bar2), Delta \= =;
		compare(Delta, Beat1, Beat2)), !.


sameSongs(Song1, Song2)	:-
	songsDiff(Song1, Song2, Surplus1, Surplus2),
	(Surplus1 = [], Surplus2 = [], !;
		writeln('surplus 1:'),
		writeTree(Surplus1),
		writeln('surplus 2:'),
		writeTree(Surplus2),
		fail).
songsDiff(Song1, Song2, Surplus1, Surplus2) :-
	retractall(notationDb(_, _, _, _)),
	loadData(Song1),
	mvNotation(1),
	loadData(Song2),
	mvNotation(2),
	rmSameNotation,
	findall(notation(Time, Tone, Dur), notationDb(1, Time, Tone, Dur),
		Surplus1),
	findall(notation(Time, Tone, Dur), notationDb(2, Time, Tone, Dur),
		Surplus2).
mvNotation(Db) :-
	notation(Time, Tone, Dur),
	assertz(notationDb(Db, Time, Tone, Dur)),
	fail.
mvNotation(_).
rmSameNotation :-
	notationDb(1, Time, Tone, Dur),
	notationDb(2, Time, Tone, Dur),
	retractall(notationDb(1, Time, Tone, Dur)),
	retractall(notationDb(2, Time, Tone, Dur)),
	fail.
rmSameNotation.


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

pitchShift(Shift, ((Bar, Beat, Staff), Pitch1, Duration),
	((Bar, Beat, Staff), Pitch2, Duration)) :-
	intervalDiff(Pitch1, Pitch2, Shift).

