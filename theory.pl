:- module(theory, [
	allSongChords/1,
	probSymbolChord/2
	]).

/** <module> Music theory

This module covers music theory relations.

It uses following data types forms:
$ Tone : _tone_ or _|(tone, octave)|_, e.g. =c= or =|(c, 1)|=
$ Chord : _|[tones]|_, e.g. =|[c, (e, 2)]|=
$ Scale : _|(root, interval pattern)|_, e.g. =|(fis, major)|=
$ Func: _|harmonic function|_, e.g. =1= (tonic)
$ Time : _|(measure, beat)|_ or _|(measure, beat, _)|_, e.g. =|(10, 2)|=
$ Beats : _|beats|_, e.g. =3=
$ Duration : e.g. =1= (a whole note), =4= (a quarter note)
*/

:- use_module(aux).
:- use_module(data).

:- [
	'theory/basics',
	'theory/harmony',
	'theory/intervals',
	'theory/scales',
	'theory/symbolChords',
	'theory/time'
	].

