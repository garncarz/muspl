:- module(theory, [
	toneFromScale/2,
	chordFromScale/2,
	
	scaleTone/3,
	scaleChord/3,
	scaleSong/2,
	
	timeDiff/3,
	durationToBeats/2,
	toneAtTime/2,
	chordAtTime/2
	]).

/** <module> Music theory

This module covers music theory relations.

We use following data types forms:
$ Tone : _tone_ or _|(tone, octave)|_, e.g. =c= or =|(c, 1)|=
$ Chord : _|[tones]|_, e.g. =|[c, (e, 2)]|=
$ Scale : _|(root, interval pattern)|_, e.g. =|(fis, major)|=
$ Time : _|(measure, beat)|_ or _|(measure, beat, _)|_, e.g. =|(10, 2)|=
$ Beats : _|beats|_, e.g. =3=
$ Duration : e.g. =1= (a whole note), =4= (a quarter note)
*/

:- use_module(data).

%% rest(-Rest)
% True if Rest stands for a rest in notation.
rest(r).
rest(s).

%% scale(-Scale, -Tones)
% Scale consists of Tones (mere names).
scale((c, major), [c, d, e, f, g, a, b]).
scale((d, minor), [d, e, f, g, a, bes, c]).
scale((f, major), [f, g, a, bes, c, d, e, f]).

%% toneFromScale(-Tone, -Scale)
% True if Tone is from Scale.
toneFromScale(Tone, Scale) :-
	scale(Scale, ScaleTones),
	(member(Tone, ScaleTones);
		Tone = (ToneName, _),
		member(ToneName, ScaleTones)).

%% chordFromScale(+Chord, -Scale)
% True if Chord is from Scale.
chordFromScale(Chord, Scale) :-
	length(Chord, Length), Length > 0,
	scale(Scale, _),
	forall(member(Tone, Chord), toneFromScale(Tone, Scale)).


scaleTone(Scale, Tone, 1) :-
	toneFromScale(Tone, Scale), !.
scaleTone(Scale, Tone, 0) :-
	scale(Scale, _), not(toneFromScale(Tone, Scale)), !.


avg_list(List, Avg) :-
	length(List, Len), Len > 0,
	sum_list(List, Sum),
	Avg is Sum / Len.

scaleChord(Scale, Chord, Fuzzy) :-
	scale(Scale, _),
	maplist(scaleTone(Scale), Chord, ToneFuzzies),
	avg_list(ToneFuzzies, Fuzzy).

scaleSong(Scale, Fuzzy) :-
	scale(Scale, _),
	allBeats(Beats),
	maplist(chordAtTime, Chords, Beats),
	maplist(scaleChord(Scale), Chords, ChordFuzzies),
	avg_list(ChordFuzzies, Fuzzy).


%% timeDiff(+Time1, +Time2, -Diff)
% True if Time2 - Time1 = Diff in beats.
timeDiff(Time1, Time2, Diff) :-
	once((Time1 = (Measure1, Beat1, _); Time1 = (Measure1, Beat1))),
	once((Time2 = (Measure2, Beat2, _); Time2 = (Measure2, Beat2))),
	once(timeSignature(BeatsPerMeasure, _)),
	Diff is (Measure2 - Measure1) * BeatsPerMeasure + Beat2 - Beat1.


%% durationToBeats(+Duration, -Beats)
% True if Duration(s) take Beats of beats.
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

