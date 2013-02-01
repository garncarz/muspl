:- module(lilypond, [exportLy/1]).

/** <module> Lilypond export
*/

:- use_module(data).
:- use_module(theory).

:- dynamic notationL/3.

:- ['lilypond.plt'].

%% chord(+Start, -Chord, +Duration)
% True if Chord consisting of _Pitches_, length as Duration starts at Start.
%
% @param Chord (Start, -_Pitches_, Duration)
chord(Start, (Start, Pitches, Duration), Duration) :-
	findall(Pitch, notationL(Start, Pitch, Duration), Pitches).

%% chords(+Start, -Chords)
% True if Chords (and nothing else) start at Start.
chords(Start, Chords) :-
	findall(Duration, notationL(Start, _, Duration), Durs1),
	sort(Durs1, Durs2),
	maplist(chord(Start), Chords, Durs2).

%% staffLine(+Staff, -Line)
% True if Staff is made of music elements Line.
staffLine(Staff, Line) :-
	nextChord(Staff, Chord),
	retractChord(Chord),
	conflictingChords(Chord, Conflicting),
	(Conflicting == [] -> Item = Chord;
		Item = [Chord | Conflicting]),
	staffLine(Staff, Rest),
	Line = [Item | Rest].
staffLine(_, []).

%% pitchLily(+Tone, -Lily)
% True if Tone is represented by Lily string.
%
% @param Tone _|(Pitch, Octave)|_
pitchLily((Pitch, Octave), Lily) :-
	Octave > 0 -> Octave2 is Octave - 1, pitchLily((Pitch, Octave2), Lily2),
		concat(Lily2, '\'', Lily);
	Octave < 0 -> Octave2 is Octave + 1, pitchLily((Pitch, Octave2), Lily2),
		concat(Lily2, ',', Lily);
	Lily = Pitch.

unfoldDurs(Duration, Action, Result) :-
	number(Duration) -> call(Action, Duration, Result);
	Duration = [Dur1] -> call(Action, Dur1, Result);
	Duration = [Dur1 | DursRest],
	call(Action, Dur1, Res1),
	unfoldDurs(DursRest, Action, ResultsRest),
	atomic_list_concat([Res1, ' ~', ResultsRest], Result).

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
	unfoldDurs(Duration, atomic_concat(Type), RestLily).

conflictingChords(Chord, [Chord2 | RestChords]) :-
	findConflictingChordTo(Chord, Chord2),
	conflictingChords(Chord, RestChords).
conflictingChords(Chord, []) :-
	not(findConflictingChordTo(Chord, _)).

findConflictingChordTo(Chord, Conflicting) :-
	Chord = ((_, _, Staff), _, _),
	nextChord(Staff, Conflicting),
	conflictChords(Chord, Conflicting),
	retractChord(Conflicting).

nextChord(Staff, Chord) :-
	allBeats(Staff, Beats),
	member(Start, Beats),
	chords(Start, Chords),
	member(Chord, Chords),
	
	% still exists: @tbd polish
	Chord = (_, _, Duration),
	chord(Start, Chord, Duration).

conflictChords(Chord1, Chord2) :-
	Chord1 \= Chord2,
	Chord1 = (Start1, _, Duration1),
	Chord2 = (Start2, _, _),
	sameStaff(Start1, Start2),
	timeDiff(Start1, Start2, Diff),
	Diff >= 0,
	durationToBeats(Duration1, Beats1),
	Diff < Beats1.

spaceFiller(From, To, Filler) :-
	timeDiff(From, To, Diff),
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

%% itemLily(+Item, -ItemLily)
% True if music element Item is represented by ItemLily string.
%
% @param Item chord or rest
itemLily(Item, ItemLily) :- 
	voicesLily(Item, ItemLily);
	chordLily(Item, ItemLily);
	restLily(Item, ItemLily).

%% staffLily(+Staff, -StaffLily)
% Renders a staff line into a complete Lilypond line.
%
% @param Staff Possible values: =g= or =f=
staffLily(Staff, String) :-
	notationScale((Root, IntervalPattern)),
	timeSignature(BeatsInBar, BeatUnit),
	(Staff == 'g', Clef = 'treble';
		Staff == 'f', Clef = 'bass'),
	atomic_list_concat(['staff', Staff, ' = { \\clef ', Clef, ' \\key ',
		Root, ' \\', IntervalPattern, ' \\time ', BeatsInBar, '/', BeatUnit,
		'\n'], '', Header),
	
	staffLine(Staff, StaffLine),
	maplist(itemLily, StaffLine, LilyItems),
	atomic_list_concat(LilyItems, ' ', LilyLine),
	
	atomic_list_concat([Header, LilyLine, '\n}\n\n'], '', String).

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

copyNotation :-
	retractall(notationL/3),
	fail.
copyNotation :-
	notation(Start, What, Dur),
	assertz(notationL(Start, What, Dur)),
	fail.
copyNotation.

retractChord((Start, _, Duration)) :-
	retractall(notationL(Start, _, Duration)).

%% exportLy(+Filename)
% Exports notation into a Lilypond file.
exportLy(Filename) :-
	copyNotation,
	
	symbolChordsLily(SymChords),
	staffLily(g, StaffG),
	staffLily(f, StaffF),
	
	tell(Filename),
	maplist(write, [
		'\\version "2.16.1"\n\n',
		SymChords,
		StaffG,
		StaffF,
		'\\score { <<\n',
		'\t\\new ChordNames { \\set chordChanges = ##t \\symChords }\n',
		'\t\\new PianoStaff << ',
		'\\new Staff \\staffg ',
		'\\new Staff \\stafff >>',
		'\n>>\n\\layout { }\n}\n\n',
		'\\score { <<\n',
		'\t\\new Staff { \\set Staff.midiInstrument = #"church organ" ',
			'\\symChords}\n',
		'\t\\new PianoStaff << ',
		'\\new Staff \\staffg ',
		'\\new Staff \\stafff >>',
		'\n>>\n\\midi { }\n}\n\n'
	]),
	told, !.

