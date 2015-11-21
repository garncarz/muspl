:- begin_tests(tone).

test(toneToIntervalToC1, [set(Tone == [tone{pitch:dis, octave:1},
		tone{pitch:es, octave:1}])]) :-
	toneToIntervalToC(Tone, 15).
test(toneToIntervalToC2, [set(Diff == [(-20, -1)])]) :-
	toneToIntervalToC(tone{pitch:es, octave:(-2)}, Diff).

test(diff) :-
	tone{pitch:d}.diff(tone{pitch:g}) == 5,
	tone{pitch:g}.diff(tone{pitch:d}) == -5,
	tone{pitch:ais, octave:1}.diff(tone{pitch:b, octave:(-1)}) == -23.

test(add) :-
	once((tone{pitch:b, octave:(-1)} :< tone{pitch:a, octave:0}.add(-10))),
	once((tone{pitch:dis, octave:0} :< tone{pitch:dis, octave:0}.add(0))),
	once((tone{pitch:g}.add(3) == tone{pitch:ais})),
	once((tone{pitch:c}.add(7:1) == tone{pitch:gis})),
	once((tone{pitch:c}.add(7:(-1)) == tone{pitch:ges})).

test(pitchFreq) :-
    tone{pitch:a, octave:0}.pitchFreq() == 440,
    Freq2 = tone{pitch:c, octave:1}.pitchFreq(), abs(Freq2 - 523.25) < 0.1,
    Freq3 = tone{pitch:as, octave:(-2)}.pitchFreq(), abs(Freq3 - 103.83) < 0.1.

:- end_tests(tone).
