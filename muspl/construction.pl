:- module(construction, [
    process/0,

    % copying:
    copyBars/5, copyBars/3, copyBar/2, copyBarsCond/4,

    % conditions:
    isStaff/2, true/1,

    % transformations:
    pitchShift/3
    ]).

:- use_module(data).

process :-
    retractall(notation(_, _, _)),
    fail.
process :-
    retract(m Action),
    process(Action),
    fail.
process.
process([]).
process([FirstAction | Rest]) :-
    process(FirstAction),
    process(Rest), !.
process(Value = Eval) :-
    assertz(mDb(Value, Eval)).
process(Action) :-
    melody{} :< Action,
    member(Key, [pitch, len]),
    get_dict(Key, Action, Value),
    mDb(Value, Eval),
    ActionEval = Action.put(Key, Eval),
    process(ActionEval), !.
process(Action) :-
    melody{start:(Bar, Beat, Staff), relative:(Pitch, Octave, Dur1)} :< Action,
    multiplied(Action, pitch, PitchDiff, RestPitchDiff),
    (multiplied(Action, len, Len, RestLen); Len = 1, RestLen = []),
    (
        is_list(Len) -> Dur = duration{len:Len};
        is_dict(Len, exact), exact{dur:ExactDur} :< Len ->
            Dur = duration{len:ExactDur};
        Dur = duration{len:Dur1}.mul(Len)
    ),
    Time = position{bar: Bar, beat:Beat, staff:Staff},
    (PitchDiff \= r ->
        extra Scale, is_dict(Scale, scale),
        % TODO Tone = Scale.add(tone{pitch: Pitch, octave:Octave}, PitchDiff),
        PitchShift = Scale.intAtFrom(PitchDiff, Pitch),
        Tone = tone{pitch: Pitch, octave:Octave}.add(PitchShift),
        assertz(notation(Time, Tone, Dur));
        true
    ),
    Time2 = Time.add(Dur), !,
    ActionRest = Action
        .put(start, (Time2.bar, Time2.beat, Time2.staff))
        .put(pitch, RestPitchDiff)
        .put(len, RestLen),
    process(ActionRest).
process(Action) :-
    copyBars{from:Bar1, to:Bar2} :< Action,
    (copyBars{count:Count} :< Action; Count = 1),
    (copyBars{cond:Cond} :< Action; Cond = true),
    (copyBars{action:Subaction} :< Action; Subaction = =),
    copyBars(Bar1, Bar2, Count, Cond, Subaction), !.
process(Action) :-
    del{cond:Cond} :< Action,
    notation(Time, Tone, Dur),
    call(Cond, (Time, Tone, Dur)),
    retract(notation(Time, Tone, Dur)),
    fail.
process(_).  % TODO maybe error


multiplied(Action, Key, Value, Rest) :-
    get_dict(Key, Action, [Value*Times | Rest1]) ->
        (Times > 1 ->
            RestTimes is Times - 1,
            Rest = [Value*RestTimes | Rest1];
        Rest = Rest1);
    get_dict(Key, Action, [Value | Rest]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COPYING:

copyBars(Start, Dest, Len, Cond, Action) :-
    DestEnd is Dest + Len - 1,
    between(Dest, DestEnd, Bar),
    Bar1 is Bar - (Dest - Start),
    notation(Time1, Pitch, Duration),
    Time1.bar = Bar1,
    Time21 = Time1.put(bar, Bar),
    call(Cond, (Time1, Pitch, Duration)),
    call(Action, (Time21, Pitch, Duration), (Time2, Pitch2, Duration2)),
    assertz(notation(Time2, Pitch2, Duration2)),
    fail.
copyBars(_, _, _, _, _).

copyBars(Start, Dest, Len) :-
    copyBars(Start, Dest, Len, true, =).

copyBar(Start, Dest) :-
    copyBars(Start, Dest, 1).

copyBarsCond(Start, Dest, Len, Cond) :-
    copyBars(Start, Dest, Len, Cond, =).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONDITIONS:

isStaff(Staff, (Time1, _, _)) :- Time1.staff = Staff.
true(_).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSFORMATIONS:

pitchShift(Shift, (Time, Tone1, Duration), (Time, Tone2, Duration)) :-
    once((Tone2 = Tone1.add(Shift); Tone2 = Tone1)).
