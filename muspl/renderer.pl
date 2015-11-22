:- module(renderer, [renderSong/1]).

% very slow, probably because of extensive use of retract/assert

:- use_module(helpers).
:- use_module(data).
:- use_module(audioExport).

:- dynamic
    toneSamples/1,
    songSamples/1,
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

addSongSample(Frame, Sample) :-
    retract(songSamples(Frame-Sample0)),
    Sample2 is Sample0 + Sample,
    asserta(songSamples(Frame-Sample2)), !.

addSongSamples(StartPos, Samples) :-
    durationToFrames(StartPos.elapsed(), StartFrame),
    length(Samples, Len),
    forall(between(1, Len, Index),
           (nth1(Index, Samples, Sample),
            Frame is StartFrame + Index,
            addSongSample(Frame, Sample))), !.

renderSong :-
    retractall(toneSamples(_)),
    retractall(songSamples(_)),
    retractall(songLength(_)),
    asserta(songLength(duration{len:0})),
    fail.
renderSong :-
    notation(Pos, _Tone, Dur),
    End = Pos.add(Dur).elapsed(),
    once((songLength(Len), End.cmp(Len) = '>' ->
        retract(songLength(_)),
        asserta(songLength(End));
        true
    )),
    fail.
renderSong :-
    songLength(Len),
    format('total length: ~w~n', [Len]),
    durationToFrames(Len, Frames),
    forall(between(0, Frames, Frame),
           asserta(songSamples(Frame-0))),
    fail.
renderSong :-
    notation(Pos, Tone, Dur),
    format('rendering ~w~n', [(Pos, Tone, Dur)]),
    renderTone(Tone, Dur, Samples),
    format('adding~n'),
    addSongSamples(Pos, Samples),
    format('added~n'),
    fail.
renderSong.
renderSong(Filename) :-
    renderSong,
    findall(FS, songSamples(FS), FramedSamples),
    keysort(FramedSamples, SortedFramedSamples),
    pairs_values(SortedFramedSamples, Samples),
    exportWav(Filename, Samples).
