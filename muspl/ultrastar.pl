:- module(ultrastar, [exportUS/1, exportUS/2]).

:- use_module(helpers).
:- use_module(data).

pitchValue(Tone, Value) :-
	Value is 60 + tone{pitch:c, octave:1}.diff(Tone).

usTime(Time, USTime) :-
	USTime is round(Time * 2).
usDur(Dur, USDur) :-
	USDur is round(Dur * 1.1).

processElements([], [], _).
processElements([], [eol], _).
processElements([], [eol, eol], _).
processElements(Notes, [eol | LyricsRest], LastEnd) :-
	Notes = [(Time, _, _) | _],
	timeDiff(time{bar:1, beat:1}, Time, Start1),
	usTime(Start1, Start),
	
	ChangeTime is round((LastEnd + Start) / 2),

	maplist(write, ['- ', ChangeTime, '\n']),
	
	processElements(Notes, LyricsRest, ChangeTime).
processElements(Notes, Lyrics, _) :-
	Notes = [(Time, Tone, Dur) | NotesRest],
	Lyrics = [syl(Syllable) | LyricsRest],
	
	timeDiff(time{bar:1, beat:1}, Time, Start1),
	usTime(Start1, Start),
	
	durationToBeats(Dur, DurInBeats1),
	usDur(DurInBeats1, DurInBeats),
	
	pitchValue(Tone, Pitch),
	
	maplist(write, [
		': ', Start, ' ', DurInBeats, ' ', Pitch, ' ', Syllable, '\n']),
	
	EndTime is Start + DurInBeats,
	processElements(NotesRest, LyricsRest, EndTime).
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
	(extra tempo(Tempo); Tempo = 100),
	
	USTempo is round(Tempo / 2),
	
	maplist(write, [
		'#TITLE:', Title, '\n',
		'#ARTIST:', Artist, '\n',
		'#MP3:', Audioname, '\n', 
		'#BPM:', USTempo, '\n'
		]),
	
	findall((Time, Tone, Dur),
		(notation(Time, Tone, Dur), Tone \= r, Time.staff = v),
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

