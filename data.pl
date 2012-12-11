:- module(data, [
	notation/3,
	timeSignature/2,
	notationScale/1,
	
	loadData/1,
	clearData,
	
	allBeats/2,
	allBeats/1,
	
	posCmp/3
	]).

:- dynamic
	notation/3,
	timeSignature/2,
	notationScale/1.

loadData(Name) :-
	clearData,
	string_concat('data/', Name, Filename),
	consult(Filename).

clearData :-
	retractall(notation(_, _, _)),
	retractall(timeSignature(_, _)),
	retractall(notationScale(_)).

%% allBeats(+Staff, -Beats)
% True if Staff contains music elements at Beats (sorted).
%
% @tbd All Beats (including exceeded) should be included.
allBeats(Staff, Beats) :-
	findall((Bar, Beat, Staff), notation((Bar, Beat, Staff), _, _), Starts),
	predsort(posCmp, Starts, Beats).

allBeats(Beats) :-
	findall((Bar, Beat), notation((Bar, Beat, _), _, _), Starts),
	predsort(posCmp, Starts, Beats).

%% posCmp(-Delta, +Time1, +Time2)
% True if Time1 compared to Time2 is Delta.
posCmp(Delta, (Bar1, Beat1, Staff), (Bar2, Beat2, Staff)) :-
	posCmp(Delta, (Bar1, Beat1), (Bar2, Beat2)), !.

posCmp(Delta, (Bar1, Beat1), (Bar2, Beat2)) :-
	number(Beat1), number(Beat2), once((
	Bar1 < Bar2, Delta = <;
	Bar1 > Bar2, Delta = >;
	Beat1 < Beat2, Delta = <;
	Beat1 > Beat2, Delta = >;
	Delta = =)).

