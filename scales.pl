:- module(scales, [
	scale/2,
	scaleAt/3
	]).

:- use_module(aux).
:- use_module(basics).
:- use_module(musicTime).

:- ['scales.plt'].

dbScale((ces, major), [ces, des, es, fes, ges, as, bes]).
dbScale((ges, major), [ges, as, bes, ces, des, es, f]).
dbScale((des, major), [des, es, f, ges, as, bes, c]).
dbScale((as, major), [as, bes, c, des, es, f, g]).
dbScale((es, major), [es, f, g, as, bes, c, d]).
dbScale((bes, major), [bes, c, d, es, f, g, a]).
dbScale((f, major), [f, g, a, bes, c, d, e]).
dbScale((c, major), [c, d, e, f, g, a, b]).
dbScale((g, major), [g, a, b, c, d, e, fis]).
dbScale((d, major), [d, e, fis, g, a, b, cis]).
dbScale((a, major), [a, b, cis, d, e, fis, gis]).
dbScale((e, major), [e, fis, gis, a, b, cis, dis]).
dbScale((b, major), [b, cis, dis, e, fis, gis, ais]).
dbScale((fis, major), [fis, gis, ais, b, cis, dis, eis]).
dbScale((cis, major), [cis, dis, eis, fis, gis, ais, bis]).

dbScale((as, minor), [as, bes, ces, des, es, fes, ges]).
dbScale((es, minor), [es, f, ges, as, bes, ces, des]).
dbScale((bes, minor), [bes, c, des, es, f, ges, as]).
dbScale((f, minor), [f, g, as, bes, c, des, es]).
dbScale((c, minor), [c, d, es, f, g, as, bes]).
dbScale((g, minor), [g, a, bes, c, d, es, f]).
dbScale((d, minor), [d, e, f, g, a, bes, c]).
dbScale((a, minor), [a, b, c, d, e, f, g]).
dbScale((e, minor), [e, fis, g, a, b, c, d]).
dbScale((b, minor), [b, cis, d, e, fis, g, a]).
dbScale((fis, minor), [fis, gis, a, b, cis, d, e]).
dbScale((cis, minor), [cis, dis, e, fis, gis, a, b]).
dbScale((gis, minor), [gis, ais, b, cis, dis, e, fis]).
dbScale((dis, minor), [dis, eis, fis, gis, ais, b, cis]).
dbScale((ais, minor), [ais, bis, cis, dis, eis, fis, gis]).

%% scale(-Scale, -Tones)
% Scale consists of Tones (mere names).
scale(scale{root:Root, quality:Quality}, Tones) :-
	dbScale((Root, Quality), Tones).

%% toneFromScale(-Tone, -Scale)
% Tone is from Scale.
toneFromScale(Tone, Scale) :-
	is_dict(Tone, tone),
	scale(Scale, ScaleTones),
	member(Tone.pitch, ScaleTones).

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
fuzzyScaleCmp(Delta, (Scale1, Fuzzy1), (Scale2, Fuzzy2)) :- once((
	compare(Delta, Fuzzy1, Fuzzy2), Delta \= =;
	compare(Delta, Scale1.root, Scale2.root), Delta \= =;
	compare(Delta, Scale1.quality, Scale2.quality))).

