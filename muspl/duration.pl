:- module(duration, [
	dursInvCmp/3
	]).

:- use_module(helpers).
:- use_module(data).

:- ['duration.plt'].

Dur1.mul(Mul) := Dur2 :-
	durationToBeats(Dur1.len, Beats1),
	Beats2 is Beats1 * Mul,
	beatsToDuration(Beats2, Len2),
	Dur2 = Dur1.put(len, Len2).

Dur.beats() := Beats :-
	Dur.get(negative) ->
		durationToBeats(Dur.len, Beats2),
		Beats is -Beats2;
	durationToBeats(Dur.len, Beats).

Dur1.add(Dur2) := Dur3 :-
	sumDurationsInv(Dur1.len, SumInvDur1),
	sumDurationsInv(Dur2.len, SumInvDur2),
	DurSum is 1 / (SumInvDur1 + SumInvDur2),
	normalizedDuration(DurSum, DurLen),
	Dur3 = Dur1.put(len, DurLen).

%% durationToBeats(+Duration, -Beats)
% True if Duration(s) take Beats of beats.
durationToBeats(0, 0) :- !.
durationToBeats(Duration, Beats) :-
	number(Duration), Duration > 0,
	timeSignature(_, NoteDuration),
	Beats is NoteDuration / Duration.
durationToBeats([], 0).
durationToBeats([Duration | Rest], Beats) :-
	durationToBeats(Duration, Beat1),
	durationToBeats(Rest, BeatsR),
	Beats is Beat1 + BeatsR.

Dur0.fromBeats(Beats) := Dur :-
	Beats < 0 ->
		Beats2 is -Beats,
		beatsToDuration(Beats2, DurLen),
		Dur = Dur0.put(len, DurLen).put(negative, true);
	beatsToDuration(Beats, DurLen),
	Dur = Dur0.put(len, DurLen).

beatsToDuration(0, 0) :- !.
beatsToDuration(Beats, Duration) :-
	number(Beats), Beats > 0,
	timeSignature(_, NoteDuration),
	FloatDuration is NoteDuration / Beats,
	normalizedDuration(FloatDuration, Duration).

Dur.norm() := Normalized :-
	Dur.len = [_ | _] ->
		Normalized = Dur.len;
	normalizedDuration(Dur.len, Normalized).

normalizedDuration(Duration, Normalized) :-
	(Duration > 128; Duration < 10 * epsilon) -> Normalized = [];

	NormDur is max(2 ** ceiling(log(Duration) / log(2)), 1),
	(NormDur =:= Duration -> Normalized = [NormDur];
		DurRest is 1 / (1 / Duration - 1 / NormDur),
		normalizedDuration(DurRest, NormRest),
		Normalized = [NormDur | NormRest]).

sumDurationsInv(Durs, Sum) :-
	map_list(inverse, Durs, Invs),
	sum_list(Invs, Sum).

dursInvCmp(Delta, Durs1, Durs2) :-
	sumDurationsInv(Durs1, Sum1),
	sumDurationsInv(Durs2, Sum2),
	compare(Delta, Sum2, Sum1).

