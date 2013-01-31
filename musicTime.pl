:- module(musicTime, [
	simpleTime/2,
	timeDiff/3,
	durationToBeats/2,
	beatsToDuration/2,
	allSongChords/1,
	allSongChordsWithDur/1
	]).

:- ['musicTime.plt'].

simpleTime(Time, (Bar, Beat)) :-
	once((Time = (Bar, Beat, _Staff);
		Time = (Bar, Beat))).

%% timeDiff(+Time1, +Time2, -Diff)
% True if Time2 - Time1 = Diff in beats.
timeDiff(Time1, Time2, Diff) :-
	simpleTime(Time1, (Measure1, Beat1)),
	simpleTime(Time2, (Measure2, Beat2)),
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
	durationToBeats(Duration, Beat1),
	durationToBeats(Rest, BeatsR),
	Beats is Beat1 + BeatsR.

beatsToDuration(Beats, Duration) :-
	number(Beats), Beats > 0,
	timeSignature(_, NoteDuration),
	FloatDuration is NoteDuration / Beats,
	normalizedDuration(FloatDuration, Duration).

normalizedDuration(Duration, Normalized) :-
	(Duration > 128; Duration < 10 * epsilon) -> Normalized = [];
	
	NormDur is 2 ** ceiling(log(Duration) / log(2)),
	(NormDur =:= Duration -> Normalized = [NormDur];
		DurRest is 1 / (1 / Duration - 1 / NormDur),
		normalizedDuration(DurRest, NormRest),
		Normalized = [NormDur | NormRest]).

durationDiff(Time1, Time2, Diff) :-
	timeDiff(Time1, Time2, TimeDiff),
	beatsToDuration(TimeDiff, Diff).

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

first((A, _), A).
afterEndBeat(EndBeat) :-
	allBeats(Beats),
	maplist(first, Beats, Measures),
	max_list(Measures, MaxMeasure),
	
	once(timeSignature(BeatsPerMeasure, _)),
	AfterBeat is BeatsPerMeasure + 1,
	
	EndBeat = (MaxMeasure, AfterBeat).

%% allSongChords(-Chords)
% Returns all song's chords as they follow.
allSongChords(Chords) :-
	allBeats(Beats),
	maplist(chordAtTime, Chords, Beats).
allSongChordsWithDur(DurChords) :-
	allBeats(Beats),
	
	afterEndBeat(EndBeat),
	append(Beats, [EndBeat], NextBeats),
	maplist(durationDiff, [(1, 0) | Beats], NextBeats, Durs1),
	Durs1 = [_ | Durs],

	maplist(chordAtTime, Chords, Beats),
	
	maplist(zip, Chords, Durs, DurChords).
zip(A, B, (A, B)).

