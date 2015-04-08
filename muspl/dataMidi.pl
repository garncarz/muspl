:- module(dataMidi, [importMidi/1]).

:- use_module(data).
:- use_module(midi).

:- ['dataMidi.plt'].

toneFromMidi(60, (c, 1)).
toneFromMidi(61, (cis, 1)).
toneFromMidi(62, (d, 1)).
toneFromMidi(63, (dis, 1)).
toneFromMidi(64, (e, 1)).
toneFromMidi(65, (f, 1)).
toneFromMidi(66, (fis, 1)).
toneFromMidi(67, (g, 1)).
toneFromMidi(68, (gis, 1)).
toneFromMidi(69, (a, 1)).
toneFromMidi(70, (ais, 1)).
toneFromMidi(71, (b, 1)).
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
	Beat is floor((TrueBeats - Measure * MeasureBeats) / MeasureBeats * 6) + 1,
	TrueMeasure is Measure + 1,
	Time = (TrueMeasure, Beat).


findToneEnding(Chan, Tone, Track, Ending) :-
	Track = [(Time, Event) | RestTrack],
	(Event = (noteOn, Chan, Tone, Vol), Vol == 0 ->
		Ending = Time;
	findToneEnding(Chan, Tone, RestTrack, Ending)).

parseTones([], []).
parseTones(Track, Tones) :-
	Track = [(Beats, Event) | RestTrack],
	parseTones(RestTrack, RestTones),
	(Event = (noteOn, Chan, MTone, Vol), Vol > 0, Chan \= 10 ->
		findToneEnding(Chan, MTone, RestTrack, Ending),
		beatsTime(Beats, (Measure, Beat)),
		toneFromMidi(MTone, Tone),
		(MTone > 59 -> Staff = g; Staff = f),
		Time = (Measure, Beat, Staff),
		Dur2 is Ending - Beats,
		Dur1 is 1 / (Dur2 / 4),
		normalizedDuration(Dur1, Dur),
		ToneNotation = (Time, Tone, Dur),
		Tones = [ToneNotation | RestTones];
	Tones = RestTones).


addNotation((Time, Tone, Dur)) :-
	assertz(notation(Time, Tone, Dur)).

importMidi(Filename) :-
	readMidi(Filename, Tracks, TPB),
	maplist(trackToAbsBeats(TPB), Tracks, AbsTracks),
	maplist(parseTones, AbsTracks, Tones),
	flatten(Tones, AllTones),
	clearData,
	assertz(notationScale((f, major))),  % TODO generalise
	assertz(timeSignature(6, 8)),
	maplist(addNotation, AllTones),
	retractRedundant,
	!.

