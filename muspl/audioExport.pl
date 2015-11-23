:- module(audioExport, [exportWav/2, sampleRate/1]).

numChannels(1).
% sampleRate(44100).
sampleRate(8000).
bitsPerSample(24).

bytesCount(BytesCount) :-
    bitsPerSample(BitsPerSample),
    BytesCount is BitsPerSample / 8.

% see https://ccrma.stanford.edu/courses/422/projects/WaveFormat/

writeChar(Char) :- put_char(wav, Char).
writeByte(Byte) :- put_byte(wav, Byte).
bytePart(X, Part, Byte) :-
    Shift is (Part - 1) * 8,
    Byte is (X /\ (255 << Shift)) >> Shift.
writeWord(Word, Size) :-
    forall(between(1, Size, Part),
        (bytePart(Word, Part, Byte),
        writeByte(Byte))).
writeWord(Word) :- writeWord(Word, 2).
writeDWord(DWord) :- writeWord(DWord, 4).
writeString(String) :-
    string_chars(String, Chars),
    forall(member(Char, Chars), writeChar(Char)).
writeSample(Sample) :-
    bytesCount(BytesCount),
    SampleWord is truncate(Sample * 255 ** BytesCount),
    writeWord(SampleWord, BytesCount).


writeRiffHeader :-
    writeString("RIFF"),
    writeDWord(0), % TODO change in the end
    writeString("WAVE").

writeFmtSubchunk :-
    numChannels(NumChannels),
    sampleRate(SampleRate),
    bitsPerSample(BitsPerSample),
    ByteRate is SampleRate * NumChannels * BitsPerSample / 8,
    BlockAlign is NumChannels * BitsPerSample / 8,

    writeString("fmt "),
    writeDWord(16),
    writeWord(1), % AudioFormat
    writeWord(NumChannels),
    writeDWord(SampleRate),
    writeDWord(ByteRate),
    writeWord(BlockAlign),
    writeWord(BitsPerSample).

writeDataSubchunkHeader :-
    writeString("data"),
    writeDWord(0). % TODO change in the end


writeSamples(Samples) :-
    forall(member(Sample, Samples), writeSample(Sample)).


exportWav(Filename, Samples) :-
    open(Filename, write, File, [type(binary), alias(wav)]),

    writeRiffHeader,
    writeFmtSubchunk,
    writeDataSubchunkHeader,

    writeSamples(Samples),

    close(File).
