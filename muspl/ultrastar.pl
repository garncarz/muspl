:- module(ultrastar, [exportUS/1, exportUS/2]).

:- use_module(helpers).
:- use_module(data).

pitchValue(Tone, Value) :-
    Value is 60 + tone{pitch:c, octave:1}.diff(Tone).

usPos(Pos, USPos) :-
    USPos is round(Pos.beats() * 2).
usDur(Dur, USDur) :-
    USDur is round(Dur.beats() * 1.1).

processElements([], [], _).
processElements([], [eol], _).
processElements([], [eol, eol], _).
processElements(Notes, [eol | LyricsRest], LastEnd) :-
    Notes = [(Pos, _, _) | _],
    usPos(Pos.elapsed(), Start),

    ChangePos is round((LastEnd + Start) / 2),

    maplist(write, ['- ', ChangePos, '\n']),

    processElements(Notes, LyricsRest, ChangePos).
processElements(Notes, Lyrics, _) :-
    Notes = [(Pos, Tone, Dur) | NotesRest],
    Lyrics = [syl(Syllable) | LyricsRest],

    usPos(Pos.elapsed(), Start),
    usDur(Dur, DurInBeats),
    pitchValue(Tone, Pitch),

    maplist(write, [
        ': ', Start, ' ', DurInBeats, ' ', Pitch, ' ', Syllable, '\n']),

    EndPos is Start + DurInBeats,
    processElements(NotesRest, LyricsRest, EndPos).
processElements(Notes, Lyrics, LastEnd) :-
    verbose(notClean:(Notes, Lyrics, LastEnd)).


exportUS(Name) :-
    atomic_concat(Name, '.txt', Filename),
    atomic_concat(Name, '.ogg', Audioname),
    exportUS(Filename, Audioname).
exportUS(Filename, Audioname) :-
    tell(Filename),

    extra title(Title),
    (extra artist(Artist); extra composer(Artist)),
    (extra tempo(Tempo); Tempo = 120),

    USTempo is round(Tempo / 2),

    maplist(write, [
        '#TITLE:', Title, '\n',
        '#ARTIST:', Artist, '\n',
        '#MP3:', Audioname, '\n',
        '#BPM:', USTempo, '\n'
        ]),

    findall((Pos, Tone, Dur),
        (notation(Pos, Tone, Dur), Tone \= r, Pos.staff = v),
        Notes1),
    predsort(timeCmp, Notes1, Notes),

    extra lyrics(v, LyricsStream),
    phrase(lyrics(Lyrics), LyricsStream),

    processElements(Notes, Lyrics, 0),

    write('E'),

    told, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LYRICS:

:- use_module(library(dcg/basics)).

underscore --> "_".
underscores --> underscore, !, underscores.
underscores --> [].

elem(syl(~)) --> whites, underscore.
elem(syl(Syl)), Ahead -->
    whites,
    string_without(" -_\n", Core),
    (underscores; []),
    ("-", { Space = '', Ahead = [] };
        whites, "_", { Space = '', Ahead = "_" };
        white, { Space = ' ', Ahead = [] };
        [], { Space = '', Ahead = [] }),
    { Core \= [],
        string_to_atom(Core, CoreAtom),
        atomic_list_concat([CoreAtom, Space], Syl)
    }.
elem(eol) --> "\n".

lyrics([Elem | Rest]) --> elem(Elem), lyrics(Rest).
lyrics([]) --> [].
