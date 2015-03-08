# MusPl, a musical Prolog library

This is a beginning project aiming at logical construction/analysis of musical pieces.
It's written in [SWI-Prolog](http://www.swi-prolog.org) (version 7, using dicts).
So far the construction phase is being developed.

## Simple example

Load and export Flies's Lullaby (included in the project) as a Lilypond file
and a karaoke text file.

```prolog
$ swipl
?- [muspl].
true.

?- loadData('examples/wiegenlied').
true.

?- exportLy('wiegenlied.ly').
maxCount:144
count:100
count:0
true.

?- exportUS('wiegenlied.txt', 'wiegenlied.ogg').
true.

?- halt.
```

Now run [Lilypond](http://www.lilypond.org) to convert it to
[MIDI](http://garncarz.github.io/muspl/wiegenlied.midi)
and [PDF](http://garncarz.github.io/muspl/wiegenlied.pdf).

```
$ lilypond wiegenlied.ly
```

![PDF screen](http://garncarz.github.io/muspl/wiegenlied.png)

You can use [TiMidity++](http://timidity.sourceforge.net)
or some other MIDI renderer to convert the MIDI to OGG (or MP3, if you like).

```
$ timidity -Ov wiegenlied.midi -o wiegenlied.ogg
```

If you use [UltraStar Deluxe](http://sourceforge.net/projects/ultrastardx)
as your karaoke program, just copy
the [TXT](http://garncarz.github.io/muspl/wiegenlied.txt)
and [OGG](http://garncarz.github.io/muspl/wiegenlied.ogg) files
to the program's `songs` folder and sing it!


## Construction

Simple melody line:

```prolog
extra scale{root:f, quality:major}.  % scale of a song

m melody{start:(1, 1, v),  % bar 1, beat 1, staff 'v' (e.g. voice)
	relative:(a, 1, 8),  % tone a, octave 1, duration 1/8
	pitch:[0, 1, 0, -1, -2, -1, -2],  % relative pitches from the scale
	len:[1, 1, 1, 1, 1, 1, 2]}.  % multiples of the relative duration
```

The same line with lengths described briefly:

```prolog
m melody{start:(1, 1, v), relative:(a, 1, 8),
	pitch:[0, 1, 0, -1, -2, -1, -2], len:[1*6, 2]}.
```

Repeated pitch/length sequences can be reused by a name:

```prolog
m [
	'6long' = [1*6, [4, 8, 4]],  % [4, 8, 4] = exact duration 1/4 + 1/8 + 1/4
	melody{start:(5, 1, v), relative:(g, 1, 8),
		pitch:[0, -1:1, 0, 0, -1:1, 0, 2], len:'6long'},  % :1 = sharp, :(-1) would be flat
	melody{start:(5, 1, g), relative:(e, 1, 8),
		pitch:[0, -1:1, 0, 0, -1:1, 0, 2], len:'6long'}
].
```

Bars can be copied, under a condition and transformed in a way:

```prolog
m copyBars{from:6, to:10, count:2, cond:isStaff(f), action:pitchShift(5)}.
```
