:- module(data, [
	op(500, fx, extra),
	op(500, fx, cond),

	notation/3,
	extra/1,
	cond/2,
	
	loadData/1,
	saveData/1,
	clearData,
	
	sameStaff/2,
	
	retractRedundant,
	
	allStaffs/1,
	allBeats/2,
	allBeats/1,
	
	timeCmp/3,
	
	sameSongs/2,
	songsDiff/4
	]).

:- op(-1, fx, extra).
:- op(-1, fx, cond).

:- dynamic
	notation/3,
	extra/1,
	cond/2,
	
	notationDb/4.

:- discontiguous
	notation/3,
	extra/1,
	cond/2.

:- use_module(aux).
:- use_module(construction).

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
	retractall(extra(_)),
	retractall(cond(_, _)).

sameStaff((_, _, Staff), (_, _, Staff)).

retractRedundant :-
	findall(notation(Time, Tone, Dur), notation(Time, Tone, Dur), Notation),
	sort(Notation, NotationSorted),
	retractall(notation(_, _, _)),
	multiAssert(NotationSorted).

staffCmp(Delta, Staff1, Staff2) :-
	Sorted = [v, g, f, b],
	nth0(Index1, Sorted, Staff1),
	nth0(Index2, Sorted, Staff2),
	compare(Delta, Index1, Index2).
allStaffs(Staffs) :-
	findall(Staff, notation((_, _, Staff), _, _), AllStaffsTeam),
	predsort(staffCmp, AllStaffsTeam, Staffs).

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

