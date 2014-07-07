:- begin_tests(data).

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

:- end_tests(data).

