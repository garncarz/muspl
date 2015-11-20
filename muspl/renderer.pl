:- module(renderer).

:- use_module(helpers).
:- use_module(data).
:- use_module(audioExport).

secondsPerBeat(0.6).
durationToSeconds(Dur, Seconds) :-
    secondsPerBeat(SecondsPerBeat),
    Seconds is Dur.beats() * SecondsPerBeat.

renderTone(Note, Samples) :-
    Note = (_Start, Tone, Dur),
    is_dict(Tone, tone),
    Freq = Tone.pitchFreq(),
    durationToSeconds(Dur, Seconds),
    sampleRate(SampleRate),
    MaxTime is round(SampleRate * Seconds),
    findall(
        Sample,
        (
            between(0, MaxTime, Time),
            Sample is sin(2 * pi * Time / SampleRate * Freq)
                      * exp(log(0.05) * Time / SampleRate / Seconds) / 4
        ),
        Samples).

renderSong(Filename) :-
    findall(Samples,
        (notation(Start, Tone, Dur), renderTone((Start, Tone, Dur), Samples)),
        AllSamples),
    flatten(AllSamples, ContinuousSamples),
    exportWav(Filename, ContinuousSamples).
