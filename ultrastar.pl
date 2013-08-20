:- module(ultrastar, [exportUS/2]).

pitchValue(Tone, Value) :-
	intervalDiff((c, 1), Tone, Diff),
	Value is 60 + Diff.

usTime(Time, USTime) :-
	USTime is round(Time * 2).

noteElement((Start, DurInBeats, Pitch, Lyrics)) :-
	notation((Bar, Beat, v), Tone, Dur),
	Tone \= r,
	timeDiff((1, 1), (Bar, Beat), Start1),
	durationToBeats(Dur, DurInBeats1),
	
	usTime(Start1, Start),
	usTime(DurInBeats1, DurInBeats),
	
	pitchValue(Tone, Pitch),
	Lyrics = 'la'.

writeElement((Start, DurInBeats, Pitch, Lyrics)) :-
	maplist(write, [
		': ', Start, ' ', DurInBeats, ' ', Pitch, ' ', Lyrics, '\n']).


exportUS(Filename, Audioname) :-
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
	
	forall(noteElement(Element), writeElement(Element)),
	
	write('E'),
	
	told, !.

