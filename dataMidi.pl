:- module(dataMidi, [loadMidi/1]).

:- use_module(data).
:- use_module(midi).
:- use_module(musicTime).

:- ['dataMidi.plt'].

toneFromMidi(60, (c, 0)).
toneFromMidi(61, (cis, 0)).
toneFromMidi(62, (d, 0)).
toneFromMidi(63, (dis, 0)).
toneFromMidi(64, (e, 0)).
toneFromMidi(65, (f, 0)).
toneFromMidi(66, (fis, 0)).
toneFromMidi(67, (g, 0)).
toneFromMidi(68, (gis, 0)).
toneFromMidi(69, (a, 0)).
toneFromMidi(70, (ais, 0)).
toneFromMidi(71, (b, 0)).
toneFromMidi(MidiTone, (Tone, Octave)) :-
	MidiTone < 60,
	MT2 is MidiTone + 12,
	toneFromMidi(MT2, (Tone, Octave2)),
	Octave is Octave2 - 1.
toneFromMidi(MidiTone, (Tone, Octave)) :-
	MidiTone > 71,
	MT2 is MidiTone - 12,
	toneFromMidi(MT2, (Tone, Octave2)),
	Octave is Octave2 + 1.

% TODO generalise
beatsTime(Beats, Time) :-
	% TrueBeats is Beats / 4,
	TrueBeats is Beats,
	MeasureBeats is 6 / 8 * 4,
	Measure is floor(TrueBeats / MeasureBeats),
	Beat is (TrueBeats - Measure * MeasureBeats) / MeasureBeats * 6 + 1,
	TrueMeasure is Measure + 1,
	Time = (TrueMeasure, Beat, g).


findToneEnding(Chan, Tone, Track, Ending) :-
	Track = [(Time, Event) | RestTrack],
	(Event = (noteOn, Chan, Tone, Vol), Vol == 0 ->
		Ending = Time;
	findToneEnding(Chan, Tone, RestTrack, Ending)).

parseTones([], []).
parseTones(Track, Tones) :-
	Track = [(Beats, Event) | RestTrack],
	parseTones(RestTrack, RestTones),
	(Event = (noteOn, Chan, MTone, Vol), Vol > 0 ->
		findToneEnding(Chan, MTone, RestTrack, Ending),
		beatsTime(Beats, Time),
		toneFromMidi(MTone, Tone),
		Dur2 is Ending - Beats,
		Dur1 is 1 / (Dur2 / 4),
		normalizedDuration(Dur1, Dur),
		ToneNotation = (Time, Tone, Dur),
		Tones = [ToneNotation | RestTones];
	Tones = RestTones).


addNotation((Time, Tone, Dur)) :-
	assertz(notation(Time, Tone, Dur)).

loadMidi(Filename) :-
	readMidi(Filename, Tracks, TPB),
	maplist(trackToAbsBeats(TPB), Tracks, AbsTracks),
	maplist(parseTones, AbsTracks, Tones),
	flatten(Tones, AllTones),
	clearData,
	assertz(notationScale((f, major))),  % TODO generalise
	assertz(timeSignature(6, 8)),
	maplist(addNotation, AllTones).

