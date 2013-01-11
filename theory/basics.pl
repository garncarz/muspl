:- ['basics.plt'].

%% rest(-Rest)
% True if Rest stands for a rest in notation.
rest(r).
rest(s).

%% toneName(+Tone, -Name)
% Tone has name Name. (An octave stripped.)
toneName(Tone, Name) :- once((
	Tone = (Name, _); Tone = Name)).

%% toneFromChord(+Tone, +Chord).
%% chordTone(+Chord, +Tone).
% Chord contains Tone.
toneFromChord(Tone, Chord) :-
	toneName(Tone, ToneName),
	maplist(toneName, Chord, ChordNames),
	member(ToneName, ChordNames), !.
chordTone(Chord, Tone) :- toneFromChord(Tone, Chord).

%% sameChords(+Chord1, +Chord2)
% True if chords contain same tones (harmonically).
sameChords(Chord1, Chord2) :-
	maplist(toneName, Chord1, Names1),
	maplist(toneName, Chord2, Names2),
	sort(Names1, Tones1),
	sort(Names2, Tones2),
	Tones1 == Tones2.

