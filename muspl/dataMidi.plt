:- begin_tests(dataMidi).

test(toneFromMidi) :-
    toneFromMidi(65, Tone1), Tone1 == (f, 1),
    toneFromMidi(36, Tone2), Tone2 == (c, -1),
    toneFromMidi(106, Tone3), Tone3 == (ais, 4).

test(importMidi) :-
    importMidi('tests/wiegenlied.midi'),

    findall((Tone, Dur),
            (notation(Pos, Tone, Dur), position{bar:1, beat:1} :< Pos),
            Tones11),
    Tones11Orig = [
        (tone{pitch:a, octave:1}, duration{len:[8]}),
        (tone{pitch:f, octave:1}, duration{len:[4]}),
        (tone{pitch:c, octave:1}, duration{len:[8]}),
        (tone{pitch:f, octave:0}, duration{len:[4]})
    ],
    subtract(Tones11, Tones11Orig, Diff11),
    subtract(Tones11Orig, Tones11, Diff11Orig),
    assertion(Diff11 == []),
    assertion(Diff11Orig == []),

    findall((Tone, Dur),
            (notation(Pos, Tone, Dur), position{bar:9, beat:3} :< Pos),
            Tones93),
    Tones93Orig = [
        (tone{pitch:d, octave:2}, duration{len:[8]}),
        (tone{pitch:ais, octave:1}, duration{len:[8]}),  % should be bes
        (tone{pitch:f, octave:1}, duration{len:[8]}),
        (tone{pitch:ais, octave:0}, duration{len:[8]})  % should be bes
    ],
    subtract(Tones93, Tones93Orig, Diff93),
    subtract(Tones93Orig, Tones93, Diff93Orig),
    assertion(Diff93 == []),
    assertion(Diff93Orig == []).

test(importSongMidi) :-
    importMidi('tests/song.midi'),
    
    % Verify that some notations were created
    findall(notation(_, _, _), notation(_, _, _), Notations),
    length(Notations, Count),
    Count > 0,
    
    % Verify that the scale is set correctly
    extra scale{root:f, quality:major},
    
    % Verify that time signature is set correctly
    extra timeSignature(6, 8),
    
    % Test that export works (should not fail)
    exportLy('/tmp/test_simple_export.ly'),
    
    % Verify at least one notation exists with proper format
    once((notation(Pos, Tone, Dur),
          Pos = position{bar:_, beat:_, staff:_},
          Tone = tone{pitch:_, octave:_},
          Dur = duration{len:_})).

:- end_tests(dataMidi).
