:- module(renderer, [renderSong/1]).

% very slow
% not using retract/assert for song samples now,
% but toneSampleAt/3 & samplesAt/2 are clumsy

:- use_module(helpers).
:- use_module(data).
:- use_module(audioExport).

:- dynamic
    toneSamples/1,
    songLength/1.

secondsPerBeat(0.6).
durationToSeconds(Dur, Seconds) :-
    secondsPerBeat(SecondsPerBeat),
    Seconds is Dur.beats() * SecondsPerBeat.
durationToFrames(Dur, Frames) :-
    durationToSeconds(Dur, Seconds),
    sampleRate(SampleRate),
    Frames is round(SampleRate * Seconds).

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

toneSampleAt((Pos, Tone, Dur), Frame, Sample) :-
    toneSamples((Tone, Dur) - Samples),
    durationToFrames(Pos.elapsed(), StartFrame),
    SampleFrame is Frame - StartFrame,
    nth0(SampleFrame, Samples, Sample).

samplesAt(Frame, Samples) :-
    findall(
        Sample,
        (
            notation(Pos, Tone, Dur),
            toneSampleAt((Pos, Tone, Dur), Frame, Sample)
        ),
        Samples).

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
    sampleRate(SampleRate),
    findall(
        Sample,
        (
            between(0, Frames, Frame),
            samplesAt(Frame, FrameSamples),
            sum_list(FrameSamples, Sample),
            (Frame mod SampleRate =:= 0 ->
                Time is Frame/SampleRate,
                Portion is Frame/Frames * 100,
                format('done: ~d s (~1f %)~n', [Time, Portion]);
                true)
        ),
        Samples).
renderSong(Filename) :-
    renderSongSamples(Samples),
    exportWav(Filename, Samples).
