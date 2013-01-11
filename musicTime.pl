:- module(musicTime, [
	allSongChords/1
	]).

:- ['musicTime.plt'].

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

%% allSongChords(-Chords)
% Returns all song's chords as they follow.
allSongChords(Chords) :-
	allBeats(Beats),
	maplist(chordAtTime, Chords, Beats).

