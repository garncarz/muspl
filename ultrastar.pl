:- module(ultrastar, [
	exportUS/3,
	loadLyrics/2
	]).

:- use_module(aux).

pitchValue(Tone, Value) :-
	intervalDiff((c, 1), Tone, Diff),
	Value is 60 + Diff.

usTime(Time, USTime) :-
	USTime is round(Time * 2).


processElements([], [], _).
processElements([], [eol], _).
processElements(Notes, [eol | LyricsRest], LastEnd) :-
	Notes = [((Bar, Beat), _, _) | _],
	timeDiff((1, 1), (Bar, Beat), Start1),
	usTime(Start1, Start),
	
	ChangeTime is round((LastEnd + Start) / 2),

	maplist(write, ['- ', ChangeTime, '\n']),
	
	processElements(Notes, LyricsRest, ChangeTime).
processElements(Notes, Lyrics, _) :-
	Notes = [((Bar, Beat), Tone, Dur) | NotesRest],
	Lyrics = [syl(Syllable) | LyricsRest],
	
	timeDiff((1, 1), (Bar, Beat), Start1),
	usTime(Start1, Start),
	
	durationToBeats(Dur, DurInBeats1),
	usTime(DurInBeats1, DurInBeats),
	
	pitchValue(Tone, Pitch),
	
	maplist(write, [
		': ', Start, ' ', DurInBeats, ' ', Pitch, ' ', Syllable, '\n']),
	
	EndTime is Start + DurInBeats,
	processElements(NotesRest, LyricsRest, EndTime).
processElements(Notes, Lyrics, LastEnd) :-
	verbose(notClean:(Notes, Lyrics, LastEnd)).


exportUS(Filename, Audioname, Lyrics) :-
	tell(Filename),
	
	extra title(Title),
	extra artist(Artist),
	extra tempo(Tempo),
	
	USTempo is round(Tempo / 2),
	
	maplist(write, [
		'#TITLE:', Title, '\n',
		'#ARTIST:', Artist, '\n',
		'#MP3:', Audioname, '\n', 
		'#BPM:', USTempo, '\n'
		]),
	
	findall(((Bar, Beat), Tone, Dur),
		(notation((Bar, Beat, v), Tone, Dur), Tone \= r),
		Notes1),
	predsort(timeCmp, Notes1, Notes),
	
	processElements(Notes, Lyrics, 0),
	
	write('E'),
	
	told, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LYRICS:

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

underscore --> "_".
underscores --> underscore, !, underscores.
underscores --> [].
elem(syl(~)) --> whites, underscore.
elem(syl(Syl)) -->
	whites,
	string_without(" -_\n", Core),
	(underscores; []),
	("-", { Space = '' };
		white, { Space = ' ' };
		[], { Space = '' }),
	{ Core \= [],
		string_to_atom(Core, CoreAtom),
		atomic_list_concat([CoreAtom, Space], Syl)
	}.
elem(eol) --> "\n".

lyrics([Elem | Rest]) --> elem(Elem), lyrics(Rest).
lyrics([]) --> [].

loadLyrics(Filename, Lyrics) :-
	phrase_from_file(lyrics(Lyrics), Filename), !.
	
