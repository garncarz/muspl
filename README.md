# MusPl, a musical Prolog library

This is a beginning project aiming at logical construction/analysis of musical pieces.
It's written in [SWI-Prolog](http://www.swi-prolog.org), version 7, which comes with a great enhancement – _dicts_.

## Simple example

Load and export Flies's Lullaby (included in the project) as a Lilypond file.

```
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

?- halt.
```

Now run [Lilypond](http://www.lilypond.org) to convert it to
[MIDI](http://garncarz.github.io/muspl/wiegenlied.midi)
and [PDF](http://garncarz.github.io/muspl/wiegenlied.pdf).

```
$ lilypond wiegenlied.ly
```

![PDF screen](http://garncarz.github.io/muspl/wiegenlied.png)

## Simple sample

This is how a melody can be described in Prolog:

```
♪ melody{start:(1, 1, g), relative:(a, 1, 8),
	pitch:[0, 1, 0, -1, -2, -1, -2],
	len:[1, 1, 1, 1, 1, 1, 2]}.
```
