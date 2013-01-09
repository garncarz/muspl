:- ['symbolChords.plt'].

dbSymbolIntervals(major, [0, 4, 7]).
dbSymbolIntervals(minor, [0, 3, 7]).
dbSymbolIntervals(augmented, [0, 4, 8]).
dbSymbolIntervals(diminished, [0, 3, 6]).
dbSymbolIntervals(sus2, [0, 2, 7]).
dbSymbolIntervals(sus4, [0, 5, 7]).


symbolChordBaseName((Root1, Quality), (Root2, Quality)) :-
	toneName(Root1, Root2).

%% symbolChord(Symbol, Chord)
%
%  @tbd Complete as much as possible.
symbolChord((Root, Quality), Tones) :-
	dbSymbolIntervals(Quality, Intervals),
	maplist(intervalDiff(Root), Tones, Intervals).
symbolChord(SymChord, Chord) :-
	symbolChordF(SymChord, Chord, 1).

symbolChordF((Root, Quality), Chord, Fuzzy) :-
	nonvar(Chord),
	length(Chord, TonesCount),
	
	(nonvar(Root), !; member(Root, Chord)),
	
	dbSymbolIntervals(Quality, Intervals),
	length(Intervals, FullCount),
	
	findall(Int, (member(Int, Intervals), member(Tone, Chord),
		intervalModDiff(Root, Tone, Int)), FoundInts),
	sort(FoundInts, UsedInts),
	length(UsedInts, UsedCount),
	
	findall(Out, (member(Tone, Chord), intervalModDiff(Root, Tone, Out),
		not(member(Out, Intervals))), FoundOuts),
	length(FoundOuts, OutCount),
	
	Fuzzy is UsedCount / FullCount - OutCount / TonesCount.

probSymbolChord(Sym, Chord) :-
	findall((Fuzzy1 - Sym1), symbolChordF(Sym1, Chord, Fuzzy1), FuzzyBag),
	keysort(FuzzyBag, Sorted),
	last(Sorted, (Fuzzy - Sym1)),
	(Fuzzy >= 0.75, symbolChordBaseName(Sym1, Sym); Sym = r).

