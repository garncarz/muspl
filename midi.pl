:- module(midi, [readMidi/3]).

keypress :- get_char(_).

readChar(Char) :- get_char(midi, Char).
readByte(Byte) :- get_byte(midi, Byte).
readWord(Word) :-
	readByte(Byte1), readByte(Byte2),
	Word is Byte1 << 8 \/ Byte2.
readTripleByte(Triple) :-
	readByte(Byte1), readByte(Byte2), readByte(Byte3),
	Triple is Byte1 << 16 \/ Byte2 << 8 \/ Byte3.
readDWord(DWord) :-
	readWord(Word1), readWord(Word2),
	DWord is Word1 << 16 \/ Word2.
readString(Length, String) :-
	findall(Char, (between(1, Length, _), readChar(Char)), Chars),
	atomic_list_concat(Chars, String).
readVar(Var) :- readVar(0, Var).
readVar(Read, Var) :-
	readByte(Byte),
	NewRead is (Read << 7) \/ (Byte /\ 127),
	(Byte >> 7 =:= 1 -> readVar(NewRead, Var);
	Var = NewRead).

readChunkID(ChunkID) :- readString(4, ChunkID).


tickPerBeat(TimeDiv, TPB) :-
	TimeDiv /\ 0x8000 =:= 0, TPB = TimeDiv.

readHeader(TracksCount, TPB) :-
	readChunkID(HeaderChunkID), HeaderChunkID == 'MThd',
	readDWord(HeaderChunkSize), HeaderChunkSize == 6,
	readWord(FormatType), FormatType == 1,
	readWord(TracksCount),
	readWord(TimeDiv), tickPerBeat(TimeDiv, TPB).


metaType(0, sequenceNumber).
metaType(1, textEvent).
metaType(2, copyrightNotice).
metaType(3, trackName).
metaType(4, instrumentName).
metaType(5, lyrics).
metaType(6, marker).
metaType(7, cuePoint).
metaType(32, channelPrefix).
metaType(47, endOfTrack).
metaType(81, setTempo).
metaType(84, smpteOffset).
metaType(88, timeSignature).
metaType(89, keySignature).
metaType(127, sequencerSpecific).
metaType(Nr, _) :- throw('unknown MIDI meta type': Nr).

metaEvent(EventType, Event) :-
	EventType == 255,
	readByte(MetaType),
	metaType(MetaType, Name),
	readVar(Length),
	(MetaType == 0 -> readByte(MSB), readByte(LSB),
		Event = (Name, MSB, LSB);
	between(1, 7, MetaType) -> readString(Length, Text),
		Event = (Name, Text);
	MetaType == 32 -> readByte(Channel),
		Event = (Name, Channel);
	MetaType == 47 -> Event = Name;
	MetaType == 81 -> readTripleByte(MPQN),
		Event = (Name, MPQN);
	MetaType == 84 -> readByte(Hour), readByte(Min), readByte(Sec),
		readByte(Frame), readByte(SubFrame),
		Event = (Name, Hour, Min, Sec, Frame, SubFrame);
	MetaType == 88 -> readByte(Numer), readByte(Denom), readByte(Metro),
		readByte(Countr32nds),
		Event = (Name, Numer, Denom, Metro, Countr32nds);
	MetaType == 89 -> readByte(Key), readByte(Scale),
		Event = (Name, Key, Scale);
	Event = Name, seek(midi, Length, current, _)).


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
ctrlType(Nr, _) :- throw('unknown MIDI control type': Nr).

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

readTrackEvents(Events) :-
	readEvent(Event),
	(endOfTrack(Event) -> Events = [Event];
	readTrackEvents(Rest), Events = [Event | Rest]).
endOfTrack((_, endOfTrack)).

readTrack(Events) :-
	readChunkID(ChunkID), ChunkID == 'MTrk',
	readDWord(_ChunkSize),
	readTrackEvents(Events).


readMidi(Filename, Tracks, TPB) :-
	open(Filename, read, File, [type(binary), alias(midi)]),
	readHeader(TracksCount, TPB),
	findall(Events, (between(1, TracksCount, _), readTrack(Events)), Tracks),
	close(File).


run :- readMidi('wiegenlied.midi', Tracks, TPB), writeln((Tracks, TPB)).

