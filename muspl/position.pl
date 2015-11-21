:- module(position, []).

:- use_module(data).

:- ['position.plt'].

% TODO compare times, like beat:2 == beat:2.0

Pos1.diff(Pos2) := Diff :-
    timeSignature(BeatsPerMeasure, _),
    DiffBeats is (Pos2.bar - Pos1.bar) * BeatsPerMeasure
        + Pos2.beat - Pos1.beat,
    Diff = duration{}.fromBeats(DiffBeats).

Pos1.add(Dur) := Pos2 :-
    timeSignature(BeatsPerMeasure, _),
    BeatsAdded = Dur.beats(),
    BeatAdded is Pos1.beat - 1 + BeatsAdded,
    Bar2 is Pos1.bar + floor(BeatAdded) div BeatsPerMeasure,
    Beat2 is floor(BeatAdded) mod BeatsPerMeasure + 1 +
        float_fractional_part(BeatAdded),
    Pos2 = Pos1.put(bar, Bar2).put(beat, Beat2).

Pos.elapsed() := position{bar:1, beat:1}.diff(Pos).

Pos1.cmp(Pos2) := Delta :-
	%(position{staff:Staff1} :< Pos1, position{staff:Staff2} :< Pos2 ->
	%	Staff1 = Staff2; true),  % TODO is it necessary?
	(compare(Delta, Pos1.bar, Pos2.bar), Delta \= =;
		compare(Delta, Pos1.beat, Pos2.beat)), !.
