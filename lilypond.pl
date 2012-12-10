:- module(lilypond, [export/1]).

/** <module> Lilypond export
*/

:- use_module(data).

%% chord(+Start, -Chord, +Duration)
% True if Chord consisting of _Pitches_, length as Duration starts at Start.
%
% @param Chord (-_Pitches_, Duration)
chord(Start, (Pitches, Duration), Duration) :-
	findall(Pitch, notation(Start, Pitch, Duration), Pitches).

%% chords(+Start, -Chords)
% True if Chords (and nothing else) start at Start.
chords(Start, Chords) :-
	findall(Duration, notation(Start, _, Duration), Durs1),
	sort(Durs1, Durs2),
	maplist(chord(Start), Chords, Durs2).

%% staffLine(+Staff, -Line)
% True if Staff is made of music elements Line.
staffLine(Staff, Line) :-
	allBeats(Staff, Beats),
	maplist(chords, Beats, Chords1),
	flatten(Chords1, Chords2),
	Line = Chords2.

%% pitchLily(+Tone, -Lily)
% True if Tone is represented by Lily string.
%
% @param Tone _|(Pitch, Octave)|_
pitchLily((Pitch, Octave), Lily) :- once((
	Octave > 0, Octave2 is Octave - 1, pitchLily((Pitch, Octave2), Lily2),
		concat(Lily2, '\'', Lily);
	Octave < 0, Octave2 is Octave + 1, pitchLily((Pitch, Octave2), Lily2),
		concat(Lily2, ',', Lily);
	Lily = Pitch)).

%% chordLily(+Chord, -ChordLily)
% True if Chord is represented by ChordLily string.
chordLily(Chord, ChordLily) :- once((
	Chord = (Pitches, Duration),
	maplist(pitchLily, Pitches, Pitches2),
	(length(Pitches, 1), [Str] = Pitches2;
		atomic_list_concat(Pitches2, ' ', StrPitches),
		atomic_list_concat(['<', StrPitches, '>'], Str)),
	(number(Duration), atomic_concat(Str, Duration, ChordLily);
		Duration = [Dur1], atomic_concat(Str, Dur1, ChordLily);
		Duration = [Dur1 | DurR], chordLily((Pitches, DurR), ChordRLily),
		atomic_list_concat([Str, Dur1, ' ~', ChordRLily], ChordLily))
	)).

%% restLily(+Rest, -RestLily)
% True if Rest is represented by RestLily string.
%
% @param Rest _|(|_=r=_|, Duration)|_ or _|(|_=|[r]|=_|, Duration)|_
restLily((r, Duration), RestLily) :- concat('r', Duration, RestLily), !.
restLily(([r], Duration), RestLily) :- concat('r', Duration, RestLily), !.

%% itemLily(+Item, -ItemLily)
% True if music element Item is represented by ItemLily string.
%
% @param Item chord or rest
itemLily(Item, ItemLily) :- chordLily(Item, ItemLily); restLily(Item, ItemLily).

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

%% export(+Filename)
% Exports notation into a Lilypond file.
export(Filename) :-
	open(Filename, write, File),
	write(File, '\\version "2.16.1"\n\n'),
	staffLily(g, StaffG),
	write(File, StaffG),
	staffLily(f, StaffF),
	write(File, StaffF),
	write(File, '\\score { \\new PianoStaff << \\new Staff \\staffg '),
	write(File, '\\new Staff \\stafff >> \\layout { } \\midi { } }\n'),
	close(File), !.


:- begin_tests(lilypond).
test(pitchLily) :-
	pitchLily((des, 2), L1), L1 = 'des\'\'',
	pitchLily((cis, 0), L2), L2 = 'cis',
	pitchLily((a, -1), L3), L3 = 'a,'.
test(chordLily) :-
	chordLily(([(a, 1), (f, 1)], 8), C1), C1 = '<a\' f\'>8',
	chordLily(([(a, 1)], 4), C2), C2 = 'a\'4'.
test(restLily) :-
	restLily((r, 2), R1), R1 = 'r2',
	restLily(([r], 4), R2), R2 = 'r4'.
:- end_tests(lilypond).

