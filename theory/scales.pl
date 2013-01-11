:- ['scales.plt'].

%% scale(-Scale, -Tones)
% Scale consists of Tones (mere names).
scale((ces, major), [ces, des, es, fes, ges, as, bes]).
scale((ges, major), [ges, as, bes, ces, des, es, f]).
scale((des, major), [des, es, f, ges, as, bes, c]).
scale((as, major), [as, bes, c, des, es, f, g]).
scale((es, major), [es, f, g, as, bes, c, d]).
scale((bes, major), [bes, c, d, es, f, g, a]).
scale((f, major), [f, g, a, bes, c, d, e]).
scale((c, major), [c, d, e, f, g, a, b]).
scale((g, major), [g, a, b, c, d, e, fis]).
scale((d, major), [d, e, fis, g, a, b, cis]).
scale((a, major), [a, b, cis, d, e, fis, gis]).
scale((e, major), [e, fis, gis, a, b, cis, dis]).
scale((b, major), [b, cis, dis, e, fis, gis, ais]).
scale((fis, major), [fis, gis, ais, b, cis, dis, eis]).
scale((cis, major), [cis, dis, eis, fis, gis, ais, bis]).

scale((as, minor), [as, bes, ces, des, es, fes, ges]).
scale((es, minor), [es, f, ges, as, bes, ces, des]).
scale((bes, minor), [bes, c, des, es, f, ges, as]).
scale((f, minor), [f, g, as, bes, c, des, es]).
scale((c, minor), [c, d, es, f, g, as, bes]).
scale((g, minor), [g, a, bes, c, d, es, f]).
scale((d, minor), [d, e, f, g, a, bes, c]).
scale((a, minor), [a, b, c, d, e, f, g]).
scale((e, minor), [e, fis, g, a, b, c, d]).
scale((b, minor), [b, cis, d, e, fis, g, a]).
scale((fis, minor), [fis, gis, a, b, cis, d, e]).
scale((cis, minor), [cis, dis, e, fis, gis, a, b]).
scale((gis, minor), [gis, ais, b, cis, dis, e, fis]).
scale((dis, minor), [dis, eis, fis, gis, ais, b, cis]).
scale((ais, minor), [ais, bis, cis, dis, eis, fis, gis]).

%% toneFromScale(-Tone, -Scale)
% Tone is from Scale.
toneFromScale(Tone, Scale) :-
	scale(Scale, ScaleTones),
	toneName(Tone, ToneName),
	member(ToneName, ScaleTones).

%% chordFromScale(+Chord, -Scale)
% True if Chord is from Scale.
chordFromScale(Chord, Scale) :-
	length(Chord, Length), Length > 0,
	scale(Scale, _),
	forall(member(Tone, Chord), toneFromScale(Tone, Scale)).

%% scaleAt(+ScaleTones, -Index, -Tone)
% ScaleTones[Index mod length(ScaleTones)] = Tone
scaleAt(ScaleTones, Index, Tone) :-
	is_list(ScaleTones), number(Index),
	length(ScaleTones, Len),
	ScaleIndex is (Index - 1) mod Len,
	nth0(ScaleIndex, ScaleTones, Tone), !.
scaleAt(ScaleTones, Index, Tone) :- 
	nth1(Index, ScaleTones, Tone).

%% scaleToneF(-Scale, +Tone, -Fuzzy)
% Tone is from Scale with fuzziness Fuzzy.
scaleToneF(Scale, Tone, 1) :-
	toneFromScale(Tone, Scale).
scaleToneF(Scale, Tone, 0) :-
	scale(Scale, _), not(toneFromScale(Tone, Scale)).

%% scaleChordF(-Scale, +Chord, -Fuzzy)
% Chord is from Scale with fuzziness Fuzzy.
scaleChordF(Scale, Chord, Fuzzy) :-
	scale(Scale, _),
	maplist(scaleToneF(Scale), Chord, ToneFuzzies),
	avg_list(ToneFuzzies, Fuzzy).

%% scaleSongF(-Scale, -Fuzzy)
% Song is of Scale with fuzziness Fuzzy.
%
% @tbd Determine scale for just a part of a song.
scaleSongF(Scale, Fuzzy) :-
	scale(Scale, _),
	allSongChords(Chords),
	maplist(scaleChordF(Scale), Chords, ChordFuzzies),
	avg_list(ChordFuzzies, Fuzzy).

%% sortedSongScales(-Scales)
% Returns song's possible Scales, sorted in descending order.
sortedSongScales(Scales) :-
	findall((Scale, Fuzzy), scaleSongF(Scale, Fuzzy), L1),
	predsort(fuzzyScaleCmp, L1, L2),
	reverse(L2, Scales).
fuzzyScaleCmp(Delta, ((Root1, Intervals1), Fuzzy1),
	((Root2, Intervals2), Fuzzy2)) :- once((
	compare(Delta, Fuzzy1, Fuzzy2), Delta \= =;
	compare(Delta, Root1, Root2), Delta \= =;
	compare(Delta, Intervals1, Intervals2))).

