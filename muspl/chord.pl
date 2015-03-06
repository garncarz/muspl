:- module(chord, []).

:- ['chord.plt'].

dbQualityIntervals(major, [0, 4, 7]).
dbQualityIntervals(minor, [0, 3, 7]).
dbQualityIntervals(augmented, [0, 4, 8]).
dbQualityIntervals(diminished, [0, 3, 6]).
dbQualityIntervals(sus2, [0, 2, 7]).
dbQualityIntervals(sus4, [0, 5, 7]).

dbQualityIntervals(major7, [0, 4, 7, 11]).
dbQualityIntervals(minor7, [0, 3, 7, 10]).
dbQualityIntervals(majorMinor7, [0, 4, 7, 10]).
dbQualityIntervals(diminished7, [0, 3, 6, 9]).
dbQualityIntervals(augmented7, [0, 4, 8, 10]).
dbQualityIntervals(halfDiminished7, [0, 3, 6, 10]).
dbQualityIntervals(minorMajor7, [0, 3, 7, 11]).

dbQualityIntervals(major6, [0, 4, 7, 9]).
dbQualityIntervals(minor6, [0, 3, 7, 9]).

dbQualityIntervals(dominant9, [0, 4, 7, 10, 13]).
dbQualityIntervals(major9, [0, 4, 7, 11, 14]).
dbQualityIntervals(minor9, [0, 3, 7, 10, 14]).

/*
dbQualityIntervals(dominant11,
dbQualityIntervals(major11,
dbQualityIntervals(minor11,

dbQualityIntervals(dom9maj13,
dbQualityIntervals(dom11maj13,
dbQualityIntervals(major13,
dbQualityIntervals(minor13,
*/


chord{root:Root, quality:Quality}.has(Tone) := true :-
	is_dict(Tone, tone),
	dbQualityIntervals(Quality, Intervals),
	member(Int, Intervals),
	tone{pitch:Root}.modDiff(Tone, Int), !.
chord{root:Root, quality:Quality}.has(Tone) := true :-
	var(Tone),
	dbQualityIntervals(Quality, Intervals),
	member(Int, Intervals),
	Tone = tone{pitch:Root}.add(Int).
Chord.has(Tones) := true :-
	is_list(Tones),
	forall(member(Tone, Tones), Chord.has(Tone)).


chord{}.tones(Tones) := Chord :-
	member(PotRoot, Tones),
	Root = PotRoot.pitch,
	
	dbQualityIntervals(Quality, _Intervals),
	
	Chord = chord{root:Root, quality:Quality},
	Chord.has(Tones).

