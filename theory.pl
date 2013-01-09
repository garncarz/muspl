:- module(theory, [
	toneFromScale/2,
	chordFromScale/2,
	
	scaleTone/3,
	scaleChord/3,
	allSongChords/1,
	scaleSong/2,
	harmonicFuncChord/3,
	
	sortedSongScales/1,
	
	timeDiff/3,
	durationToBeats/2,
	toneAtTime/2,
	chordAtTime/2,
	
	probSymbolChord/2
	]).

/** <module> Music theory

This module covers music theory relations.

It uses following data types forms:
$ Tone : _tone_ or _|(tone, octave)|_, e.g. =c= or =|(c, 1)|=
$ Chord : _|[tones]|_, e.g. =|[c, (e, 2)]|=
$ Scale : _|(root, interval pattern)|_, e.g. =|(fis, major)|=
$ Func: _|harmonic function|_, e.g. =1= (tonic)
$ Time : _|(measure, beat)|_ or _|(measure, beat, _)|_, e.g. =|(10, 2)|=
$ Beats : _|beats|_, e.g. =3=
$ Duration : e.g. =1= (a whole note), =4= (a quarter note)
*/

:- use_module(aux).
:- use_module(data).

:- [
	'theory/intervals',
	'theory/symbolChords'
	].

:- ['theory.plt'].

%% rest(-Rest)
% True if Rest stands for a rest in notation.
rest(r).
rest(s).

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

%% possibleFunc(-Func)
% Func is a possible harmonic function.
possibleFunc(Func) :-
	between(1, 7, Func).

%% harmonicFunc(-Func, -Members)
% Harmonic function Func has tones from scale relatively determined by Members.
% E.g. =|harmonicFunc(1, [1, 3, 5]).|=
harmonicFunc(Func, Members) :-
	once((number(Func); var(Func))),
	possibleFunc(Func),
	maplist(plus(Func), [0, 2, 4], Members).

%% toneName(+Tone, -Name)
% Tone has name Name. (An octave stripped.)
toneName(Tone, Name) :- once((
	Tone = (Name, _); Tone = Name)).

%% toneFromScale(-Tone, -Scale)
% Tone is from Scale.
toneFromScale(Tone, Scale) :-
	scale(Scale, ScaleTones),
	toneName(Tone, ToneName),
	member(ToneName, ScaleTones).

%% toneFromChord(+Tone, +Chord).
%% chordTone(+Chord, +Tone).
% Chord contains Tone.
toneFromChord(Tone, Chord) :-
	toneName(Tone, ToneName),
	maplist(toneName, Chord, ChordNames),
	member(ToneName, ChordNames), !.
chordTone(Chord, Tone) :- toneFromChord(Tone, Chord).

%% sameChords(+Chord1, +Chord2)
% True if chords contain same tones (harmonically).
sameChords(Chord1, Chord2) :-
	maplist(toneName, Chord1, Names1),
	maplist(toneName, Chord2, Names2),
	sort(Names1, Tones1),
	sort(Names2, Tones2),
	Tones1 == Tones2.

%% chordFromScale(+Chord, -Scale)
% True if Chord is from Scale.
chordFromScale(Chord, Scale) :-
	length(Chord, Length), Length > 0,
	scale(Scale, _),
	forall(member(Tone, Chord), toneFromScale(Tone, Scale)).

%% scaleAt(+ScaleTones, -Index, -Tone)
% ScaleTones[Index mod len(ScaleTones)] = Tone
scaleAt(ScaleTones, Index, Tone) :-
	is_list(ScaleTones), number(Index),
	length(ScaleTones, Len),
	ScaleIndex is (Index - 1) mod Len,
	nth0(ScaleIndex, ScaleTones, Tone), !.
scaleAt(ScaleTones, Index, Tone) :- 
	nth1(Index, ScaleTones, Tone).

%% harmonicFuncChord(-Scale, -Func, -Chord)
% Chord is of Scale's harmonic function Func.
%
% @tbd Func is 0 when unknown, 'd better fail.
harmonicFuncChord(Scale, Func, Chord) :- var(Chord),
	scale(Scale, ScaleTones),
	harmonicFunc(Func, FuncMembers),
	maplist(scaleAt(ScaleTones), FuncMembers, Chord).
harmonicFuncChord(Scale, Func, Chord) :- is_list(Chord),
	scale(Scale, ScaleTones),
	harmonicFunc(Func, FuncMembers),
	maplist(scaleAt(ScaleTones), FuncMembers, FuncChord),
	sameChords(FuncChord, Chord).
harmonicFuncChord(_, 0, _).


%% scaleTone(-Scale, +Tone, -Fuzzy)
% Tone is from Scale with fuzziness Fuzzy.
scaleTone(Scale, Tone, 1) :-
	toneFromScale(Tone, Scale).
scaleTone(Scale, Tone, 0) :-
	scale(Scale, _), not(toneFromScale(Tone, Scale)).

%% scaleChord(-Scale, +Chord, -Fuzzy)
% Chord is from Scale with fuzziness Fuzzy.
scaleChord(Scale, Chord, Fuzzy) :-
	scale(Scale, _),
	maplist(scaleTone(Scale), Chord, ToneFuzzies),
	avg_list(ToneFuzzies, Fuzzy).

%% allSongChords(-Chords)
% Returns all song's chords as they follow.
allSongChords(Chords) :-
	allBeats(Beats),
	maplist(chordAtTime, Chords, Beats).

%% scaleSong(-Scale, -Fuzzy)
% Song is of Scale with fuzziness Fuzzy.
%
% @tbd Determine scale for just a part of a song.
scaleSong(Scale, Fuzzy) :-
	scale(Scale, _),
	allSongChords(Chords),
	maplist(scaleChord(Scale), Chords, ChordFuzzies),
	avg_list(ChordFuzzies, Fuzzy).

%% sortedSongScales(-Scales)
% Returns song's possible Scales, sorted in descending order.
sortedSongScales(Scales) :-
	findall((Scale, Fuzzy), scaleSong(Scale, Fuzzy), L1),
	predsort(fuzzyScaleCmp, L1, L2),
	reverse(L2, Scales).
fuzzyScaleCmp(Delta, ((Root1, Intervals1), Fuzzy1),
	((Root2, Intervals2), Fuzzy2)) :- once((
	compare(Delta, Fuzzy1, Fuzzy2), Delta \= =;
	compare(Delta, Root1, Root2), Delta \= =;
	compare(Delta, Intervals1, Intervals2))).


%% timeDiff(+Time1, +Time2, -Diff)
% True if Time2 - Time1 = Diff in beats.
timeDiff(Time1, Time2, Diff) :-
	once((Time1 = (Measure1, Beat1, _); Time1 = (Measure1, Beat1))),
	once((Time2 = (Measure2, Beat2, _); Time2 = (Measure2, Beat2))),
	once(timeSignature(BeatsPerMeasure, _)),
	Diff is (Measure2 - Measure1) * BeatsPerMeasure + Beat2 - Beat1.


%% durationToBeats(+Duration, -Beats)
% True if Duration(s) take Beats of beats.
%
% @tbd beatsToDuration
durationToBeats(Duration, Beats) :-
	number(Duration),
	timeSignature(_, NoteDuration),
	Beats is NoteDuration / Duration.
durationToBeats([], 0).
durationToBeats([Duration | Rest], Beats) :-
	timeSignature(_, NoteDuration),
	Beat1 is NoteDuration / Duration,
	durationToBeats(Rest, BeatsR),
	Beats is Beat1 + BeatsR.


%% toneAtTime(-Tone, +Time)
% True if Tone sounds at Time.
toneAtTime(Tone, Time) :-
	notation(Time2, Tone, Duration),
	timeDiff(Time2, Time, Diff),
	Diff >= 0,
	durationToBeats(Duration, Beats),
	Diff < Beats.


%% chordAtTime(-Chord, +Time)
% True if Chord (and no more tones) sound at Time.
chordAtTime(Chord, Time) :-
	findall(Tone, toneAtTime(Tone, Time), Chord).

