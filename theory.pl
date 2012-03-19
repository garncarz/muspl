rest(r).
rest(s).

scale((c, major), [c, d, e, f, g, a, b]).
scale((d, minor), [d, e, f, g, a, bes, c]).

toneFromScale(Tone, Scale) :-
	scale(Scale, ScaleTones),
	(member(Tone, ScaleTones);
		Tone = (ToneName, _),
		member(ToneName, ScaleTones)).
chordFromScale(Chord, Scale) :-
	forall(member(Tone, Chord), toneFromScale(Tone, Scale)).

timeDiff(Time1, Time2, Diff) :-
	once((Time1 = (Measure1, Beat1, _); Time1 = (Measure1, Beat1))),
	once((Time2 = (Measure2, Beat2, _); Time2 = (Measure2, Beat2))),
	timeSignature(BeatsPerMeasure, _),
	Diff is (Measure2 - Measure1) * BeatsPerMeasure + Beat2 - Beat1.
durationToBeats(Duration, Beats) :-
	number(Duration),
	timeSignature(_, NoteDuration),
	Beats is NoteDuration / Duration.
durationToBeats([], 0).
durationToBeats([Duration | Rest], Beats) :-
	timeSignature(_, NoteDuration),
	Beat1 is NoteDuration / Duration,
	durationToBeats(Rest, BeatsR),
	Beats is Beat1 + BeatsR.

toneAtTime(Tone, Time) :-
	notation(Time2, Tone, Duration),
	timeDiff(Time2, Time, Diff),
	Diff >= 0,
	durationToBeats(Duration, Beats),
	Diff < Beats.
chordAtTime(Chord, Time) :-
	findall(Tone, toneAtTime(Tone, Time), Chord).

:- begin_tests(theory).
test(toneFromScale) :-
	toneFromScale(g, (c, major)),
	not(toneFromScale(eis, (c, major))).
:- end_tests(theory).

