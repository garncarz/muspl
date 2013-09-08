:- module(renderer).

:- use_module(aux).

:- use_module(data).
:- use_module(intervals).
:- use_module(musicTime).

:- use_module(audioExport).

pitchFreq(Pitch, Freq) :-
	intervalDiff((a, 0), Pitch, Diff),
	Freq is 440 * 2 ^ (Diff / 12).


secondsPerBeat(0.6).
durationToSeconds(Dur, Seconds) :-
	durationToBeats(Dur, Beats),
	secondsPerBeat(SecondsPerBeat),
	Seconds is Beats * SecondsPerBeat.


renderTone(Note, Samples) :-
	Note = (_Start, Pitch, Dur),
	pitchFreq(Pitch, Freq),
	durationToSeconds(Dur, Seconds),
	sampleRate(SampleRate),
	MaxTime is round(SampleRate * Seconds),
	findall(Sample,
		(between(0, MaxTime, Time),
			Sample is sin(2 * pi * Time / SampleRate * Freq)
				* exp(log(0.05) * Time / SampleRate / Seconds) / 4),
		Samples).


renderSong(Filename) :-
	findall(Samples,
		(notation(Start, Pitch, Dur), renderTone((Start, Pitch, Dur), Samples)),
		AllSamples),
	flatten(AllSamples, ContinuousSamples),
	exportWav(Filename, ContinuousSamples).


:- begin_tests(renderer).

test(pitchFreq) :-
	pitchFreq((a, 0), Freq1), Freq1 == 440,
	pitchFreq((c, 1), Freq2), abs(Freq2 - 523.25) < 0.1,
	pitchFreq((as, -2), Freq3), abs(Freq3 - 103.83) < 0.1.

:- end_tests(renderer).

