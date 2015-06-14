:- module(position, []).

:- use_module(data).

:- ['position.plt'].

% TODO compare times, like beat:2 == beat:2.0

Pos1.diff(Pos2) := Diff :-
	timeSignature(BeatsPerMeasure, _),
	Diff is (Pos2.bar - Pos1.bar) * BeatsPerMeasure
		+ Pos2.beat - Pos1.beat.

Pos1.add(Dur) := Pos2 :-
	timeSignature(BeatsPerMeasure, _),
	durationToBeats(Dur, BeatsAdded),
	BeatAdded is Pos1.beat - 1 + BeatsAdded,
	Bar2 is Pos1.bar + floor(BeatAdded) div BeatsPerMeasure,
	Beat2 is floor(BeatAdded) mod BeatsPerMeasure + 1 +
		float_fractional_part(BeatAdded),
	Pos2 = Pos1.put(bar, Bar2).put(beat, Beat2).
