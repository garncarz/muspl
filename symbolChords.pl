:- module(symbolChords, [
	symbolChord/2,
	probSymbolChord/2
	]).

:- use_module(basics).
:- use_module(intervals).

:- ['symbolChords.plt'].

dbSymbolIntervals(major, [0, 4, 7]).
dbSymbolIntervals(minor, [0, 3, 7]).
dbSymbolIntervals(augmented, [0, 4, 8]).
dbSymbolIntervals(diminished, [0, 3, 6]).
dbSymbolIntervals(sus2, [0, 2, 7]).
dbSymbolIntervals(sus4, [0, 5, 7]).

dbSymbolIntervals(major7, [0, 4, 7, 11]).
dbSymbolIntervals(minor7, [0, 3, 7, 10]).
dbSymbolIntervals(majorMinor7, [0, 4, 7, 10]).
dbSymbolIntervals(diminished7, [0, 3, 6, 9]).
dbSymbolIntervals(augmented7, [0, 4, 8, 10]).
dbSymbolIntervals(halfDiminished7, [0, 3, 6, 10]).
dbSymbolIntervals(minorMajor7, [0, 3, 7, 11]).

dbSymbolIntervals(major6, [0, 4, 7, 9]).
dbSymbolIntervals(minor6, [0, 3, 7, 9]).

dbSymbolIntervals(dominant9, [0, 4, 7, 10, 13]).
dbSymbolIntervals(major9, [0, 4, 7, 11, 14]).
dbSymbolIntervals(minor9, [0, 3, 7, 10, 14]).

/*
dbSymbolIntervals(dominant11,
dbSymbolIntervals(major11,
dbSymbolIntervals(minor11,

dbSymbolIntervals(dom9maj13,
dbSymbolIntervals(dom11maj13,
dbSymbolIntervals(major13,
dbSymbolIntervals(minor13,
*/

symbolChordBaseName((Root1, Quality), (Root2, Quality)) :-
	toneName(Root1, Root2).

%% symbolChord(Symbol, Chord)
%
%  @tbd Complete as much as possible.
symbolChord(SymChord, Tones) :-
	is_dict(SymChord),
	SymChord :< chord{root:Root, quality:Quality},
	dbSymbolIntervals(Quality, Intervals),
	maplist(intervalDiff(Root), Tones, Intervals).
symbolChord(SymChord, Chord) :-
	symbolChordF(SymChord, Chord, 1).

symbolChordF(chord{root:Root, quality:Quality}, Chord, Fuzzy) :-
	nonvar(Chord),
	length(Chord, TonesCount),
	
	(nonvar(Root), !; member(Root, Chord);
		member(PotRoot, Chord), Root = PotRoot.pitch),
	
	dbSymbolIntervals(Quality, Intervals),
	length(Intervals, FullCount),
	
	findall(Int, (member(Int, Intervals), member(Tone, Chord),
		intervalModDiff(Root, Tone, Int)), FoundInts),
	sort(FoundInts, UsedInts),
	length(UsedInts, UsedCount),
	
	findall(Out, (member(Tone, Chord), intervalModDiff(Root, Tone, Out),
		not((member(AmodInt, Intervals), AmodInt mod 12 =:= Out))), FoundOuts),
	length(FoundOuts, OutCount),
	
	Fuzzy is UsedCount / FullCount - OutCount / TonesCount.

probSymbolChord(Sym, Chord) :-
	findall((Fuzzy1 - Sym1), symbolChordF(Sym1, Chord, Fuzzy1), FuzzyBag),
	keysort(FuzzyBag, Sorted),
	last(Sorted, (Fuzzy - Sym1)),
	(Fuzzy >= 0.75, symbolChordBaseName(Sym1, Sym); Sym = r).

