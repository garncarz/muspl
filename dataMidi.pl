:- module(dataMidi, [load/1]).

:- use_module(midi).

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
	TrueBeats is Beats / 4,
	writeln(TrueBeats),
	MeasureBeats is 6 / 8 * 4,
	writeln(MeasureBeats),
	Measure is floor(TrueBeats / MeasureBeats),
	Beat is (TrueBeats - Measure * MeasureBeats) / MeasureBeats * 6,
	RealMeasure is Measure + 1,
	Time = (RealMeasure, Beat).


load(Filename) :-
	readMidi(Filename, Tracks, TPB),
	maplist(trackToAbsBeats(TPB), Tracks, AbsTracks),
	writeln(AbsTracks).

