:- module(lilypond, [
	aux/0
	]).

/** <module> Lilypond export
*/


%% chord(+Start, ?Chord, +Duration)
% True if Chord consisting of _Pitches_, length as Duration starts at Start.
%
% @param Chord (?_Pitches_, Duration)
chord(Start, (Pitches, Duration), Duration) :-
	findall(Pitch, notation(Start, Pitch, Duration), Pitches).

%% chords(+Start, ?Chords)
% True if Chords (and nothing else) start at Start.
chords(Start, Chords) :-
	findall(Duration, notation(Start, _, Duration), Durs1),
	sort(Durs1, Durs2),
	maplist(chord(Start), Chords, Durs2).

%% allBeats(+Staff, ?Beats)
% True if Staff contains music elements at Beats (sorted).
%
% @tbd All Beats (including exceeded) should be included.
allBeats(Staff, Beats) :-
	findall((Bar, Beat, Staff), notation((Bar, Beat, Staff), _, _), Starts),
	predsort(posCmp, Starts, Beats).

%% staffLine(+Staff, ?Line)
% True if Staff is made of music elements Line.
staffLine(Staff, Line) :-
	allBeats(Staff, Beats),
	maplist(chords, Beats, Chords1),
	flatten(Chords1, Chords2),
	Line = Chords2.

%% pitchLily(+Tone, ?Lily)
% True if Tone is represented by Lily string.
%
% @param Tone _|(Pitch, Octave)|_
pitchLily((Pitch, Octave), Lily) :- once((
	Octave > 0, Octave2 is Octave - 1, pitchLily((Pitch, Octave2), Lily2),
		concat(Lily2, '\'', Lily);
	Octave < 0, Octave2 is Octave + 1, pitchLily((Pitch, Octave2), Lily2),
		concat(Lily2, ',', Lily);
	Lily = Pitch)).

%% chordLily(+Chord, ?ChordLily)
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

%% restLily(+Rest, ?RestLily)
% True if Rest is represented by RestLily string.
%
% @param Rest _|(|_=r=_|, Duration)|_ or _|(|_=|[r]|=_|, Duration)|_
restLily((r, Duration), RestLily) :- concat('r', Duration, RestLily), !.
restLily(([r], Duration), RestLily) :- concat('r', Duration, RestLily), !.

%% itemLily(+Item, ?ItemLily)
% True if music element Item is represented by ItemLily string.
%
% @param Item chord or rest
itemLily(Item, ItemLily) :- chordLily(Item, ItemLily); restLily(Item, ItemLily).

%% aux
% Predicate for exporting Wiegenlied into LilyPond file.
aux :-
	open('wiegenlied.ly', write, File),
	write(File, '\\version "2.14.2"\n'),
	staffLine(g, LineG1),
	maplist(itemLily, LineG1, LineG2),
	atomic_list_concat(LineG2, ' ', LineG),
	write(File, 'horni = { \\clef treble \\key f \\major \\time 6/8\n'),
	write(File, LineG),
	write(File, '\n}\n\n'),
	staffLine(f, LineF1),
	maplist(itemLily, LineF1, LineF2),
	atomic_list_concat(LineF2, ' ', LineF),
	write(File, 'spodni = { \\clef bass \\key f \\major \\time 6/8\n'),
	write(File, LineF),
	write(File, '\n}\n\n'),
	write(File, '\\score { \\new PianoStaff << \\new Staff \\horni \\new Staff \\spodni >> \\layout { } \\midi { } }\n'),
	close(File), !.

%% posCmp(?Delta, +Time1, +Time2)
% True if Time1 compared to Time2 is Delta.
posCmp(Delta, (Bar1, Beat1, Staff), (Bar2, Beat2, Staff)) :- once((
	Bar1 < Bar2, Delta = <;
	Bar1 > Bar2, Delta = >;
	Beat1 < Beat2, Delta = <;
	Beat1 > Beat2, Delta = >;
	Delta = =)).


:- begin_tests(lilypond).
test(posCmp) :-
	posCmp('<', (3, 2, g), (4, 1, g)),
	posCmp('=', (10, 3, g), (10, 3, g)),
	posCmp('>', (3, 2, g), (2, 5, g)).
test(posCmpFail, [fail]) :-
	posCmp(_, (1, 1, g), (1, 1, f)).
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

