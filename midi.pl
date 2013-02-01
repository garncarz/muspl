:- module(midi, [import/1]).

keypress :- get_char(_).

readChar(Char) :- get_char(midi, Char).
readByte(Byte) :- get_byte(midi, Byte).
readWord(Word) :-
	readByte(Byte1), readByte(Byte2),
	Word is Byte1 << 8 \/ Byte2.
readDWord(DWord) :-
	readWord(Word1), readWord(Word2),
	DWord is Word1 << 16 \/ Word2.
readVar(Var) :- readVar(0, Var).
readVar(Read, Var) :-
	readByte(Byte),
	NewRead is (Read << 7) \/ (Byte /\ 127),
	(Byte >> 7 =:= 1 -> readVar(NewRead, Var);
	Var = NewRead).

readChunkID(ChunkID) :-
	findall(Char, (between(1, 4, _), readChar(Char)), Chars),
	atomic_list_concat(Chars, ChunkID).


tickPerBeat(TimeDiv, TPB) :-
	TimeDiv /\ 0x8000 =:= 0, TPB = TimeDiv.

readHeader(Tracks, TPB) :-
	readChunkID(HeaderChunkID), HeaderChunkID == 'MThd',
	readDWord(HeaderChunkSize), HeaderChunkSize == 6,
	readWord(FormatType), FormatType == 1,
	readWord(Tracks),
	readWord(TimeDiv), tickPerBeat(TimeDiv, TPB).


metaEvent(EventType, Event) :-
	EventType == 255,
	readByte(MetaType),
	(MetaType == 47 -> Event = trackEnd; Event = meta),
	readVar(Length),
	seek(midi, Length, current, _).

sysExEvent(EventType, Event) :-
	(EventType == 240; EventType == 247),
	Event = sysEx,
	readVar(Length),
	seek(midi, Length, current, _).


ctrlType(8, noteOff).
ctrlType(9, noteOn).
ctrlType(10, noteAftertouch).
ctrlType(11, controller).
ctrlType(12, programChange).
ctrlType(13, channelAfterTouch).
ctrlType(14, pitchBend).

controlEvent(EventType, Event) :-
	Type is (EventType >> 4) /\ 15,
	Chan is EventType /\ 15,
	ctrlType(Type, Name),
	readByte(Par1),
	(between(12, 13, Type) -> Event = (Name, Chan, Par1);
	readByte(Par2), Event = (Name, Chan, Par1, Par2)).


readEvent((Delta, Event)) :-
	readVar(Delta),
	readByte(EventType),
	once((metaEvent(EventType, Event);
		sysExEvent(EventType, Event);
		controlEvent(EventType, Event))).


readTrack :-
	readChunkID(ChunkID), ChunkID == 'MTrk',
	readDWord(_ChunkSize),
	repeat,
	readEvent((Delta, Event)),
	(Delta > 0 -> writeln((delta: Delta)); true),
	writeln(Event), % keypress,
	(Event == trackEnd -> !; fail).


import(Filename) :-
	open(Filename, read, File, [type(binary), alias(midi)]),
	readHeader(Tracks, TPB),
	writeln((tracks: Tracks, tpb: TPB)),
	forall(between(1, Tracks, _), readTrack),
	close(File).


run :- import('wiegenlied.midi').

