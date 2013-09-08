:- module(audioExport, [exportWav/1]).

numChannels(1).
sampleRate(44100).
bitsPerSample(8).

% see https://ccrma.stanford.edu/courses/422/projects/WaveFormat/

writeChar(Char) :- put_char(wav, Char).
writeByte(Byte) :- put_byte(wav, Byte).
bytePart(X, Part, Byte) :-
	Byte is (X /\ (255 << (Part * 8))) >> (Part * 8).
writeWord(Word) :-
	bytePart(Word, 1, Byte2),
	bytePart(Word, 0, Byte1),
	writeByte(Byte1),
	writeByte(Byte2).
writeDWord(DWord) :-
	bytePart(DWord, 3, Byte4),
	bytePart(DWord, 2, Byte3),
	bytePart(DWord, 1, Byte2),
	bytePart(DWord, 0, Byte1),
	writeByte(Byte1),
	writeByte(Byte2),
	writeByte(Byte3),
	writeByte(Byte4).
writeString(String) :-
	forall(member(Char, String), writeChar(Char)).
writeSample(Sample) :-
	bitsPerSample(BitsPerSample),
	BitsPerSample = 8,
	SampleByte is truncate((Sample + 1) * 128),
	writeByte(SampleByte).


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


writeTestSineData :-
	sampleRate(SampleRate),
	forall((MaxTime is SampleRate * 500,
		between(0, MaxTime, Time),
		Data is sin(2 * pi * Time / SampleRate * 500)),
		writeSample(Data)).


exportWav(Filename) :-
	open(Filename, write, File, [type(binary), alias(wav)]),
	
	writeRiffHeader,
	writeFmtSubchunk,
	writeDataSubchunkHeader,
	
	writeTestSineData,
	
	close(File).

