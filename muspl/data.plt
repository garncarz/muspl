:- begin_tests(data, [setup(clear), cleanup(clear)]).

:- include('testSong.plt').

test(sameStaff) :-
	sameStaff(time{bar:4, beat:1, staff:f}, time{bar:6, beat:2, staff:f}),
	not(sameStaff(time{bar:4, beat:1, staff:f}, time{bar:6, beat:2, staff:g})).

test(timeCmp1) :-
	timeCmp(<, time{bar:3, beat:2}, time{bar:4, beat:1}),
	timeCmp(=, time{bar:10, beat:3}, time{bar:10, beat:3}),
	timeCmp(>, time{bar:3, beat:2}, time{bar:2, beat:5}).
test(timeCmp2) :-
	timeCmp(<, time{bar:3, beat:2, staff:g}, time{bar:4, beat:1, staff:g}),
	timeCmp(=, time{bar:10, beat:3, staff:g}, time{bar:10, beat:3, staff:g}),
	timeCmp(>, time{bar:3, beat:2, staff:g}, time{bar:2, beat:5, staff:g}).

% TODO delete?
%test(timeCmp2Fail, [fail]) :-
%	timeCmp(_, time{bar:1, beat:1, staff:g}, time{bar:1, beat:1, staff:f}).

test(toneAtTime1, [setup(testSong), cleanup(clear),
		set(Tone == [tone{pitch:d, octave:1}, tone{pitch:bes, octave:1}])]) :-
	toneAtTime(Tone, position{bar:1, beat:2}).
test(toneAtTime2, [setup(testSong), cleanup(clear), all(Tone == [r])]) :-
	toneAtTime(Tone, position{bar:4, beat:3}).

test(chordAtTime, [setup(testSong), cleanup(clear)]) :-
	chordAtTime(Chord, position{bar:1, beat:3}),
	sort(Chord, ChordSorted), ChordSorted == [
		tone{pitch:a, octave:1}, tone{pitch:c, octave:1},
		tone{pitch:f, octave:0}, tone{pitch:f, octave:1}].

test(allSongChords, [setup(testSong), cleanup(clear)]) :-
	allSongChords(Chords),
	once((member(Chord1, Chords), Chord1 == [
		tone{pitch:f, octave:1}, tone{pitch:d, octave:1},
		tone{pitch:c, octave:0}, tone{pitch:bes, octave:0}])),
	not(member([tone{pitch:c, octave:1}, tone{pitch:f, octave:3}], Chords)).

:- end_tests(data).

