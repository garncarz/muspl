:- module(data, [
    op(500, fx, m),
    op(500, fx, extra),
    op(500, fx, cond),

    m/1,
    mDb/2,

    notation/3,
    extra/1,
    cond/2,

    loadData/1,
    saveData/1,
    clearData/0,

    sameStaff/2,

    retractRedundant/0,

    timeSignature/2,
    allStaffs/1,
    allBeats/2,
    allBeats/1,

    timeCmp/3,

    sameSongs/2,
    songsDiff/4,

    allSongChords/1
    ]).

:- op(-1, fx, m), op(-1, fx, extra), op(-1, fx, cond).

:- dynamic
    m/1,
    mDb/2,
    notation/3,
    extra/1,
    cond/2,

    notationDb/4.

:- discontiguous
    m/1,
    mDb/2,
    notation/3,
    extra/1,
    cond/2.

:- op(500, fx, m), op(500, fx, extra), op(500, fx, cond).

:- use_module(helpers).
:- use_module(construction).

:- ['data.plt'].

loadData(Name) :-
    clearData,
    directory_file_path(DataDir, DataFile, Name),
    working_directory(StartDir, DataDir),
    consult(DataFile),
    working_directory(_, StartDir),
    process.
loadOldData(Name) :-
    loadData_(Name),
    mvNotation(old),
    newNotationFrom(old).

loadLyrics(Staff, LyricsName) :-
    open(LyricsName, read, Stream, []),
    read_stream_to_codes(Stream, Lyrics),
    assertz(extra lyrics(Staff, Lyrics)).

saveData(Name) :-
    atomic_list_concat(['data/', Name, '.pl'], Filename),
    tell(Filename),
    listing(notation/3),
    told.

clearData :-
    retractall(notation(_, _, _)),
    retractall(extra(_)),
    retractall(cond(_, _)).

sameStaff(Time1, Time2) :-
    Time1.staff = Time2.staff.

retractRedundant :-
    findall(notation(Time, Tone, Dur), notation(Time, Tone, Dur), Notation),
    sort(Notation, NotationSorted),
    retractall(notation(_, _, _)),
    multiAssert(NotationSorted).

staffCmp(Delta, Staff1, Staff2) :-
    Sorted = [v, g, f, b],
    nth0(Index1, Sorted, Staff1),
    nth0(Index2, Sorted, Staff2),
    compare(Delta, Index1, Index2).
allStaffs(Staffs) :-
    findall(Staff, (notation(T, _, _), position{staff:Staff} :< T),
        AllStaffsTeam),
    predsort(staffCmp, AllStaffsTeam, Staffs).

timeSignature(BeatsPerMeasure, NoteDuration) :-
    once((extra timeSignature(BeatsPerMeasure, NoteDuration);
        BeatsPerMeasure = 4, NoteDuration = 4)).

%% allBeats(-Beats).
%% allBeats(+Staff, -Beats).
% True if notation/Staff contains music elements at Beats (sorted).
%
% @tbd All Beats (including exceeded) should be included.
allBeats(Beats) :-
    findall(Time, notation(Time, _, _), Starts),
    predsort(timeCmp, Starts, Beats).
allBeats(Staff, Beats) :-
    findall(Time, (notation(Time, _, _), position{staff:Staff} :< Time),
        Starts),
    predsort(timeCmp, Starts, Beats).

%% timeCmp(-Delta, +Time1, +Time2)
% True if Time1 compared to Time2 is Delta.
timeCmp(Delta, Elem1, Elem2) :-
    (Time1, _, _) = Elem1, (Time2, _, _) = Elem2,
    timeCmp(Delta, Time1, Time2).
timeCmp(Delta, Elem1, Elem2) :-
    _{position:Time1} :< Elem1, _{position:Time2} :< Elem2,
    timeCmp(Delta, Time1, Time2).
timeCmp(Delta, Time1, Time2) :-
    is_dict(Time1, position), is_dict(Time2, position),
    Delta = Time1.cmp(Time2).

sameSongs(Song1, Song2) :-
    songsDiff(Song1, Song2, Surplus1, Surplus2),
    (Surplus1 = [], Surplus2 = [], !;
        writeln('surplus 1:'),
        writeTree(Surplus1),
        writeln('surplus 2:'),
        writeTree(Surplus2),
        fail).
songsDiff(Song1, Song2, Surplus1, Surplus2) :-
    loadData(Song1),
    mvNotation(1),
    loadData(Song2),
    mvNotation(2),
    rmSameNotation,
    findall(notation(Time, Tone, Dur), notationDb(1, Time, Tone, Dur),
        Surplus1),
    findall(notation(Time, Tone, Dur), notationDb(2, Time, Tone, Dur),
        Surplus2).
mvNotation(Db) :-
    retractall(notationDb(Db, _, _, _)),
    fail.
mvNotation(Db) :-
    notation(Time, Tone, Dur),
    assertz(notationDb(Db, Time, Tone, Dur)),
    fail.
mvNotation(_) :- retractall(notation(_, _, _)).
rmSameNotation :-
    notationDb(1, Time, Tone, Dur),
    notationDb(2, Time, Tone, Dur),
    retractall(notationDb(1, Time, Tone, Dur)),
    retractall(notationDb(2, Time, Tone, Dur)),
    fail.
rmSameNotation.

newNotationFrom(Db) :-
    retract(notationDb(Db, (Bar, Beat, Staff), (Pitch, Octave), Dur)),
    assertz(notation(position{bar:Bar, beat:Beat, staff:Staff},
        tone{pitch:Pitch, octave:Octave}, Dur)),
    fail.
newNotationFrom(_).


% from old musicTime:

%% toneAtTime(-Tone, +Time)
% True if Tone sounds at Time.
toneAtTime(Tone, Time) :-
    notation(Time2, Tone, Duration),
    Diff = Time2.diff(Time),
    Diff.beats() >= 0,
    % TODO Duration should already be duration{}
    Diff.beats() < duration{len:Duration}.beats().

%% chordAtTime(-Chord, +Time)
% True if Chord (and no more tones) sound at Time.
chordAtTime(Chord, Time) :-
    findall(Tone, toneAtTime(Tone, Time), Chord).

%% allSongChords(-Chords)
% Returns all song's chords as they follow.
allSongChords(Chords) :-
    allBeats(Beats),
    maplist(chordAtTime, Chords, Beats).
