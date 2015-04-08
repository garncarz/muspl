:- module(lilypond, [exportLy/1]).

/** <module> Lilypond export
*/

:- use_module(helpers).
:- use_module(data).
:- use_module(duration).
:- use_module(theory).
:- use_module(tone).

:- dynamic chordsDb/1, chordsDbMaxCount/1.

:- ['lilypond.plt'].

%% chord(+Start, -Chord, +Duration)
% True if Chord consisting of _Pitches_, length as Duration starts at Start.
%
% @param Chord (Start, -_Pitches_, Duration)
chord(Start, (Start, Result, Duration), Duration) :-
	findall(Pitch, notation(Start, Pitch, Duration), Pitches),
	sort(Pitches, PitchesUnique),
	(not(member(r, PitchesUnique)) -> Result = PitchesUnique;
		(select(r, PitchesUnique, JustPitches),
		JustPitches \= [] -> Result = JustPitches;
			Result = r)).

%% chords(+Start, -Chords)
% True if Chords (and nothing else) start at Start.
chords(Start, Chords) :-
	findall(Duration, notation(Start, _, Duration), Durs1),
	predsort(dursInvCmp, Durs1, Durs2),
	maplist(chord(Start), Chords, Durs2).

createAllChordsDb(Staff) :-
	allBeats(Staff, Beats),
	maplist(chords, Beats, Chords1),
	flatten(Chords1, Chords),
	foreach(member(Chord, Chords), assertz(chordsDb(Chord))).
createAllChordsDb :-
	retractall(chordsDb(_)),
	allStaffs(Staffs),
	forall(member(Staff, Staffs), createAllChordsDb(Staff)),
	aggregate_all(count, chordsDb(_), Count),
	retractall(chordsDbMaxCount(_)),
	asserta(chordsDbMaxCount(Count)),
	verbose(maxCount:Count).

staffLine(StartTime, Line) :-
	nextChord(StartTime.staff, Chord),
	Chord = (ChordTime, _, Dur),
	retractChord(Chord),
	conflictingChords(Chord, Conflicting),
	(Conflicting == [] -> Item1 = [Chord];
		Item1 = [[Chord | Conflicting]]),
	(spaceFiller(StartTime, ChordTime, Filler) -> append([Filler], Item1, Item);
		Item = Item1),
	NextTime = ChordTime.add(Dur),
	staffLine(NextTime, Rest),
	append(Item, Rest, Line).
staffLine(_, []).

%% pitchLily(+Tone, -Lily)
% True if Tone is represented by Lily string.
pitchLily(Tone, Lily) :-
	Tone :< tone{pitch:Pitch, octave:Octave},
	(Octave > 0 -> pitchLily(Tone.lowerOctave(), Lily2),
		concat(Lily2, '\'', Lily);
	Octave < 0 -> pitchLily(Tone.higherOctave(), Lily2),
		concat(Lily2, ',', Lily);
	Lily = Pitch).

unfoldDurs(Duration, Action, Result) :-
	unfoldDurs(Duration, Action, Result, ' ~').
unfoldDurs(Duration, Action, Result, Glue) :-
	number(Duration) -> call(Action, Duration, Result);
	Duration = [Dur1] -> call(Action, Dur1, Result);
	(Duration = [Dur1, Dur2], Dur2 =:= Dur1 * 2,
		string_concat(Dur1, '.', Dur),
		call(Action, Dur, Result));
	Duration = [Dur1 | DursRest],
	call(Action, Dur1, Res1),
	unfoldDurs(DursRest, Action, ResultsRest, Glue),
	atomic_list_concat([Res1, Glue, ResultsRest], Result).

%% chordLily(+Chord, -ChordLily)
% True if Chord is represented by ChordLily string.
chordLily(Chord, ChordLily) :-
	Chord = (_, Pitches, Duration),
		maplist(pitchLily, Pitches, Pitches2),
	(length(Pitches, 1) -> [Str] = Pitches2;
		atomic_list_concat(Pitches2, ' ', StrPitches),
		atomic_list_concat(['<', StrPitches, '>'], Str)),
	unfoldDurs(Duration, atomic_concat(Str), ChordLily).

%% restLily(+Rest, -RestLily)
% True if Rest is represented by RestLily string.
%
% @param Rest _|(|_=r=_|, Duration)|_ or _|(|_=|[r]|=_|, Duration)|_
restLily((_, Rest, Duration), RestLily) :-
	((Rest == r; Rest == [r]) -> Type = r;
	(Rest == s; Rest == [s]) -> Type = s),
	unfoldDurs(Duration, atomic_concat(Type), RestLily, ' ').

conflictingChords(Chord, [Chord2 | RestChords]) :-
	findConflictingChordTo(Chord, Chord2),
	conflictingChords(Chord, RestChords).
conflictingChords(Chord, []) :-
	not(findConflictingChordTo(Chord, _)).

findConflictingChordTo(Chord, Conflicting) :-
	Chord = (Time, _, _),
	position{staff:Staff} :< Time,
	nextChord(Staff, Conflicting),
	conflictChords(Chord, Conflicting),
	retractChord(Conflicting).

nextChord(Staff, Chord) :-
	chordsDb(Chord),
	Chord = (Time, _, _),
	position{staff:Staff} :< Time.

retractChord(Chord) :-
	chordsDb(Chord),
	retractall(chordsDb(Chord)),
	aggregate_all(count, chordsDb(_), Count),
	(Count mod 100 =:= 0 -> verbose(count:Count); true).

conflictChords(Chord1, Chord2) :-
	Chord1 \= Chord2,
	Chord1 = (Start1, _, Duration1),
	Chord2 = (Start2, _, _),
	sameStaff(Start1, Start2),
	Diff = Start1.diff(Start2),
	Diff >= 0,
	durationToBeats(Duration1, Beats1),
	Diff < Beats1.

spaceFiller(From, To, Filler) :-
	Diff = From.diff(To),
	beatsToDuration(Diff, Dur),
	Filler = (_, s, Dur).

indentLily(First, Chord, Lily) :-
	First = (From, _, _),
	Chord = (To, _, _),
	(spaceFiller(From, To, Filler) ->
		maplist(itemLily, [Filler, Chord], Lilies),
		atomic_list_concat(Lilies, ' ', Lily);
		itemLily(Chord, Lily)).

voicesLily(Voices, VoicesLily) :-
	is_list(Voices),
	Voices = [First | _],
	maplist(indentLily(First), Voices, VoicesLilies),
	atomic_list_concat(VoicesLilies, ' } \\\\ { ', VL2),
	atomic_list_concat(['\n<< { ', VL2, ' } >>\n'], ' ', VoicesLily).


newBar(Item, Bar) :-
	(Item = ((Bar, 1, Staff), _, _);
	Item = [((Bar, 1, Staff), _, _) | _]),
	nonvar(Bar), nonvar(Staff).

%% itemLily(+Item, -ItemLily)
% True if music element Item is represented by ItemLily string.
%
% @param Item chord or rest
itemLily(Item, CommentedItemLily) :- 
	(newBar(Item, Bar) ->
		atomic_list_concat(['\n% Bar ', Bar, ':\n'], Prefix);
		Prefix = ''),
	(voicesLily(Item, ItemLily);
		chordLily(Item, ItemLily);
		restLily(Item, ItemLily)),
	atomic_list_concat([Prefix, ItemLily], CommentedItemLily).
	

%% staffLily(+Staff, -StaffLily)
% Renders a staff line into a complete Lilypond line.
staffLily(Staff, String) :-
	extra scale{root:Root, quality:IntervalPattern},
	extra timeSignature(BeatsInBar, BeatUnit),
	(extra clef(Staff, Clef);
		Staff == 'f' -> Clef = 'bass';
		Clef = 'treble'),
	atomic_list_concat(['{ \\clef ', Clef, ' \\key ',
		Root, ' \\', IntervalPattern, ' \\time ', BeatsInBar, '/', BeatUnit,
		'\n'], Header1),
	
	(extra tempo(Tempo) -> atomic_list_concat([Header1, '\n\\tempo ', BeatUnit,
		'=', Tempo, '\n'], Header);
		Header = Header1),
	
	staffLine(position{bar:1, beat:1, staff:Staff}, StaffLine),
	maplist(itemLily, StaffLine, LilyItems),
	atomic_list_concat(LilyItems, ' ', LilyLine),
	
	atomic_list_concat([Header, LilyLine, '\n}\n'], String1),
	
	(extra lyrics(Staff, Lyrics) ->
		string_to_atom(Lyrics, LyricsAtom),
		atomic_list_concat(['staff', Staff, ' = <<\n', String1,
			'\\addlyrics {\n', LyricsAtom, '}\n>>\n\n'], String);
	atomic_list_concat(['staff', Staff, ' = ', String1, '\n'], String)).
	

staffInstrument(Staff, Instrument) :- extra instrument(Staff, Instrument).
staffInstrument(_, 'acoustic grand').

staffInstrumentName(Staff, Name, ShortName) :-
	extra instrumentName(Staff, Name, ShortName).

% @tbd empty chord, chord's duration
dbChordQLily(major, 5).
dbChordQLily(minor, m).
dbChordQLily(augmented, aug).
dbChordQLily(diminished, dim).
dbChordQLily(major7, maj7).
dbChordQLily(minor7, m7).
dbChordQLily(majorMinor7, 7).
dbChordQLily(diminished7, dim7).
dbChordQLily(augmented7, aug7).
dbChordQLily(halfDiminished7, 'm7.5-').
dbChordQLily(minorMajor7, 'maj7.5-').
dbChordQLily(major6, 6).
dbChordQLily(minor6, m6).
dbChordQLily(dominant9, 9).
dbChordQLily(major9, maj9).
dbChordQLily(minor9, m9).
dbChordQLily(dominant11, 11).
dbChordQLily(major11, maj11).
dbChordQLily(minor11, m11).
dbChordQLily(dom9maj13, 13).
dbChordQLily(dom11maj13, '13.11').
dbChordQLily(major13, 'maj13.11').
dbChordQLily(minor13, 'm13.11').
dbChordQLily(Quality, Quality).

mergeDursByFirst([(X1, Dur1) | Rest], [], Merged) :-
	mergeDursByFirst(Rest, [(X1, Dur1)], Merged).
mergeDursByFirst([(X2, Dur2) | Rest], [(X1, Dur1) | Done], Merged) :-
	X1 == X2 ->
		addDurations(Dur1, Dur2, Dur),
		mergeDursByFirst(Rest, [(X1, Dur) | Done], Merged);
	mergeDursByFirst(Rest, [(X2, Dur2), (X1, Dur1) | Done], Merged).
mergeDursByFirst([], DoneReversed, Merged) :-
	reverse(DoneReversed, Merged).

chordSymLily((Sym, Duration), ChordLily) :-
	(Sym == r -> Root = 'r', LilQ = '';
	Sym = (Root, Quality),
	dbChordQLily(Quality, LilQ1),
	concat(':', LilQ1, LilQ)),
	unfoldDurs(Duration, chordSymLilyFormat(Root, LilQ), ChordLily).
chordSymLily(_Chord, '').
chordSymLilyFormat(Root, LilQ, Duration, Formatted) :-
	atomic_list_concat([Root, Duration, LilQ], Formatted).

symbolChordsLily(String) :-
	allSymbChordsWithDur(DurSymbChords),
	mergeDursByFirst(DurSymbChords, [], MergedDurSymbs),
	maplist(chordSymLily, MergedDurSymbs, Lilies),
	atomic_list_concat(Lilies, ' ', LiliesStr),
	atomic_list_concat(['symChords = \\chordmode { ', LiliesStr, ' }\n\n'],
		String).


writeHeader :-
	extra title(Title),
	extra composer(Composer),
	(extra poet(Poet), format(atom(PoetLine), '\tpoet = "~s"\n', [Poet]);
		PoetLine = ''),
	
	maplist(write, [
		'\\header {\n',
		'\ttitle = "', Title, '"\n',
		'\tcomposer = "', Composer, '"\n',
		PoetLine,
		'}\n\n'
		]).

%% exportLy(+Filename)
% Exports notation into a Lilypond file.
exportLy(Filename) :-
	tell(Filename),
	write('\\version "2.16.1"\n\n'),

	(writeHeader; true),

	createAllChordsDb,
	%symbolChordsLily(SymChords),
	%write(SymChords),
	
	allStaffs(Staffs),
	forall(member(Staff, Staffs),
		(staffLily(Staff, StaffLily), write(StaffLily))),
	
	write('\\score { <<\n'),
	%write('\\new ChordNames { \\set chordChanges = ##t \\symChords }\n'),
	forall(member(Staff, Staffs),
		((staffInstrumentName(Staff, Name, ShortName) ->
			atomic_list_concat(['\\set Staff.instrumentName = "', Name,
				'" \\set Staff.shortInstrumentName = "', ShortName, '" '],
				InstrString)
			; InstrString = ''),
		atomic_list_concat(['\\new Staff { ', InstrString, '\\staff', Staff,
			' }\n'], String),
		write(String))),
	write('>>\n\\layout { }\n}\n\n'),
	
	write('\\score { <<\n'),
	write('% \\new Staff { \\set Staff.midiInstrument = #"church organ" \c
			\\symChords }\n'),
	forall(member(Staff, Staffs),
		(staffInstrument(Staff, Instrument),
		atomic_list_concat(['\\new Staff { \\set Staff.midiInstrument = #"',
			Instrument, '" \\staff', Staff, ' }\n'], String),
		write(String))),
	write('>>\n\\midi { }\n}\n\n'),
	
	told, !.

