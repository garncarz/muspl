:- module(dataMidi, [importMidi/1]).

:- use_module(data).
:- use_module(midi).

:- ['dataMidi.plt'].

toneFromMidi(60, (c, 1)) :- !.
toneFromMidi(61, (cis, 1)) :- !.
toneFromMidi(62, (d, 1)) :- !.
toneFromMidi(63, (dis, 1)) :- !.
toneFromMidi(64, (e, 1)) :- !.
toneFromMidi(65, (f, 1)) :- !.
toneFromMidi(66, (fis, 1)) :- !.
toneFromMidi(67, (g, 1)) :- !.
toneFromMidi(68, (gis, 1)) :- !.
toneFromMidi(69, (a, 1)) :- !.
toneFromMidi(70, (ais, 1)) :- !.
toneFromMidi(71, (b, 1)) :- !.
toneFromMidi(MidiTone, (Tone, Octave)) :-
    MidiTone < 60, !,
    MT2 is MidiTone + 12,
    toneFromMidi(MT2, (Tone, Octave2)),
    Octave is Octave2 - 1.
toneFromMidi(MidiTone, (Tone, Octave)) :-
    MidiTone > 71, !,
    MT2 is MidiTone - 12,
    toneFromMidi(MT2, (Tone, Octave2)),
    Octave is Octave2 + 1.

% TODO generalise
beatsToPos(Beats, Pos) :-
    % TrueBeats is Beats / 4,
    TrueBeats is Beats,
    BarBeats is 6 / 8 * 4,
    Bar is floor(TrueBeats / BarBeats),
    Beat is floor((TrueBeats - Bar * BarBeats) / BarBeats * 6) + 1,
    TrueBar is Bar + 1,
    Pos = position{bar:TrueBar, beat:Beat}.


findToneEnding(Chan, Tone, Track, Ending) :-
    Track = [(Time, Event) | RestTrack],
    (Event = (noteOn, Chan, Tone, Vol), Vol == 0 ->
        Ending = Time;
    findToneEnding(Chan, Tone, RestTrack, Ending)).

parseTones([], []).
parseTones(Track, Tones) :-
    Track = [(StartBeats, Event) | RestTrack],
    parseTones(RestTrack, RestTones),
    (Event = (noteOn, Chan, MidiTone, Vol), Vol > 0, Chan \= 10 ->
        findToneEnding(Chan, MidiTone, RestTrack, EndBeats),
        beatsToPos(StartBeats, Pos1),
        toneFromMidi(MidiTone, (Pitch, Octave)),
        Tone = tone{pitch:Pitch, octave:Octave},
        (MidiTone > 59 -> Staff = g;
                          Staff = f),
        Pos = Pos1.put(staff, Staff),
        DurBeats is (EndBeats - StartBeats) * 2,  % FIXME why multiply?
        Dur = duration{}.fromBeats(DurBeats),
        ToneNotation = (Pos, Tone, Dur),
        Tones = [ToneNotation | RestTones];
    Tones = RestTones).


addNotation((Time, Tone, Dur)) :-
    assertz(notation(Time, Tone, Dur)).

importMidi(Filename) :-
    readMidi(Filename, Tracks, TPB),
    maplist(trackToAbsBeats(TPB), Tracks, AbsTracks),

    clearData,
    assertz(extra notationScale((f, major))),  % TODO generalise
    assertz(extra timeSignature(6, 8)),

    maplist(parseTones, AbsTracks, Tones),
    flatten(Tones, AllTones),
    maplist(addNotation, AllTones),
    retractRedundant,
    !.
