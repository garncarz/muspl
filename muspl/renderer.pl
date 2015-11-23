:- module(renderer, [renderSong/1]).

:- use_module(helpers).
:- use_module(data).
:- use_module(audioExport).

:- dynamic
    toneSamples/1,
    songLength/1.

:- ['renderer.plt'].

secondsPerBeat(0.6).
durationToSeconds(Dur, Seconds) :-
    secondsPerBeat(SecondsPerBeat),
    Seconds is Dur.beats() * SecondsPerBeat.
durationToFrames(Dur, Frames) :-
    durationToSeconds(Dur, Seconds),
    sampleRate(SampleRate),
    Frames is round(SampleRate * Seconds).

renderToneFrame((Tone, Dur), Frame, Sample) :-
    is_dict(Tone, tone),
    Freq = Tone.pitchFreq(),
    durationToSeconds(Dur, Seconds),
    sampleRate(SampleRate),
    Sample is sin(2 * pi * Frame / SampleRate * Freq)
              * exp(log(0.05) * Frame / SampleRate / Seconds) / 16.

renderTone(Tone, Dur, Samples) :-
    toneSamples((Tone, Dur) - Samples), !.
renderTone(Tone, Dur, Samples) :-
    is_dict(Tone, tone),
    Freq = Tone.pitchFreq(),
    durationToSeconds(Dur, Seconds),
    sampleRate(SampleRate),
    durationToFrames(Dur, MaxFrame),
    findall(
        Sample,
        (
            between(0, MaxFrame, Frame),
            Sample is sin(2 * pi * Frame / SampleRate * Freq)
                      * exp(log(0.05) * Frame / SampleRate / Seconds) / 16
        ),
        Samples),
    assertz(toneSamples((Tone, Dur) - Samples)), !.

addSamples(Samples, 0, [], Samples) :- !.
addSamples([Sample0 | SamplesRest0], 0, [Sample1 | SamplesRest1],
           [Sample2 | SamplesRest2]) :- !,
    Sample2 is Sample0 + Sample1,
    addSamples(SamplesRest0, 0, SamplesRest1, SamplesRest2).
addSamples([Sample0 | SamplesRest0], StartFrame, Samples1,
           [Sample0 | SamplesRest2]) :- !,
    StartFrame2 is StartFrame - 1,
    addSamples(SamplesRest0, StartFrame2, Samples1, SamplesRest2).
sumSamples(Samples, [], Samples) :- !.
sumSamples(Samples0, [(StartFrame, Samples1) | RestSamples], Samples) :- !,
    addSamples(Samples0, StartFrame, Samples1, Samples2),
    length(RestSamples, Missing),
    format('samples2go: ~d~n', [Missing]),
    sumSamples(Samples2, RestSamples, Samples).

renderSongSamples(_) :-
    retractall(toneSamples(_)),
    retractall(songLength(_)),
    asserta(songLength(duration{len:0})),
    fail.
renderSongSamples(_) :-
    notation(Pos, _Tone, Dur),
    End = Pos.add(Dur).elapsed(),
    once((songLength(Len), End.cmp(Len) = '>' ->
        retract(songLength(_)),
        asserta(songLength(End));
        true
    )),
    fail.
renderSongSamples(_) :-
    notation(Pos, Tone, Dur),
    format('rendering ~w~n', [(Pos, Tone, Dur)]),
    renderTone(Tone, Dur, _Samples),
    fail.
renderSongSamples(Samples) :-
    songLength(Len),
    format('total length: ~w~n', [Len]),
    durationToFrames(Len, Frames),
    findall(0, between(0, Frames, _), ZeroSamples),
    findall((StartFrame, Samples),
        (notation(Pos, Tone, Dur),
         durationToFrames(Pos.elapsed(), StartFrame),
         renderTone(Tone, Dur, Samples)
        ),
        TonesSamples),
    sumSamples(ZeroSamples, TonesSamples, Samples).
renderSong(Filename) :-
    renderSongSamples(Samples),
    exportWav(Filename, Samples).
