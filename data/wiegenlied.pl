extra title('Schlafe, mein Prinzchen, schlaf ein').
extra composer('Bernhard Flies').

extra scale{root:f, quality:major}.

extra timeSignature(6, 8).
% timeSignature(0, 6, 8). FIXME od jakého času platí

♪ melody{start:(1, 1, g), relative:(a, 1, 8), run:[0, 1, 0, -1, -2, -1, (-2, 4)]}.
♪ melody{start:(1, 1, g), relative:(f, 1, 8), run:[(0, 4), 0, -1, -2, -1]}.
♪ melody{start:(1, 1, f), relative:(c, 1, 8), run:[0, 1, 0, (-1, 4), -1]}.
♪ melody{start:(1, 1, f), relative:(f, 0, 4), run:[0, (0, 8), -3, (-3, 8)]}.

♪ melody{start:(2, 1, f), relative:(a, 0, 8), run:[0, 2, 2, 0, 2, 2]}.
♪ melody{start:(2, 1, f), relative:(f, 0, 8), run:[0, r, r, 0, r, r]}.

♪ melody{start:(2, 6, g), relative:(f, 1, 8), run:[0, 0, 3, 3, 3, 4, 5, (4, 4)]}.
♪ melody{start:(2, 6, g), relative:(f, 1, 8), run:[r, -2, 0, 0, -2, 0, 0, (0, 4)]}.
♪ melody{start:(3, 1, f), relative:(bes, 0, 8), run:[0, 2, 2, 0, 2, 2]}.
♪ melody{start:(3, 1, f), relative:(f, 0, 8), run:[0, r, r, 0, r, r]}.

♪ copyBars{from:2, to:4, cond:isStaff(f)}.


♪ melody{start:(5, 1, g), relative:(g, 1, 8), run:[0, -1:1, 0, 0,
	-1:1, 0, (2, [4, 8, 4])]}.
♪ melody{start:(5, 1, g), relative:(e, 1, 8), run:[0, -1:1, 0, 0,
	-1:1, 0, (2, [4, 8, 4])]}.
♪ melody{start:(5, 1, f), relative:(c, 0, 4), run:[0, (7, 8), 0, (7, 8)]}.
♪ melody{start:(6, 1, f), relative:(c, 0, 8), run:[0, 2, 4, 7, 4, 0]}.

♪ melody{start:(7, 1, g), relative:(a, 1, 8), run:[0, 0, 0, 1, 0, 1,
	(2, [4, 8, 4])]}.
♪ melody{start:(7, 1, g), relative:(f, 1, 8), run:[0, 0, 0, 1, 0, 1,
	(2, [4, 8, 4])]}.
♪ melody{start:(7, 1, f), relative:(f, 0, 4), run:[0, (4, 8), 0, (4, 8)]}.

♪ copyBars{from:6, to:8, cond:isStaff(f), action:pitchShift(5)}.


♪ melody{start:(9, 1, g), relative:(d, 2, 8), run:[0, 0, 0, 0,
	-1:1, 0, (2, [4, 8, 4])]}.
♪ melody{start:(9, 1, g), relative:(bes, 1, 8), run:[0, 0, 0, 0,
	0, 0, (2, [4, 8, 4])]}.
♪ melody{start:(9, 1, g), relative:(f, 1, 8), run:[0, 0, 0, 0,
	0, 0, (3, [4, 8, 4])]}.
♪ melody{start:(9, 1, f), relative:(bes, -1, 8), run:[0, 7, 7, 0, 7, 7]}.

♪ copyBars{from:9, to:10, cond:isStaff(f)}.

♪ melody{start:(11, 1, g), relative:(c, 2, 8), run:[0, 0, 0, 0,
	-1:1, 0, (3, [4, 8, 4])]}.
♪ melody{start:(11, 1, g), relative:(a, 1, 8), run:[0, 0, 0, 0,
	-1:1, 0, (2, [4, 8, 4])]}.
♪ melody{start:(11, 1, g), relative:(f, 1, 8), run:[0, 0, 0, 0,
	0, 0, (2, [4, 8, 4])]}.
♪ melody{start:(11, 1, f), relative:(f, -1, 8), run:[0, 7, 7, 0, 7, 7]}.

♪ copyBars{from:11, to:12, cond:isStaff(f)}.


♪ melody{start:(13, 1, g), relative:(bes, 1, 8), run:[0, 1, 0, -1, 0,
	-1, (-2, [4, 8, 4])]}.
♪ melody{start:(13, 1, g), relative:(g, 1, 8), run:[0, 1, 0, -1, 0,
	-1, (-2, [4, 8, 4])]}.
♪ melody{start:(13, 1, f), relative:(c, 1, 4), run:[0, (0, 8), 0, (0, 8)]}.
♪ melody{start:(13, 1, f), relative:(e, 0, 4), run:[0, (0, 8), 1, (1, 8)]}.

♪ melody{start:(14, 1, f), relative:(c, 0, 8), run:[0, 2, 4, 7, 6, 4]}.

♪ melody{start:(15, 1, g), relative:(a, 1, 8), run:[0, 1, 0, -1, 0,
	-1, (-2, [4, 8, 4])]}.
♪ melody{start:(15, 1, g), relative:(f, 1, 8), run:[0, 1, 0, -1, -1,
	-1, (-3, [4, 8, 4])]}.
♪ melody{start:(16, 1, g), relative:(a, 0, 8), run:[(0, [4, 8, 4])]}.
♪ melody{start:(15, 1, f), relative:(c, 1, 4), run:[0, (0, 8), -1, (-1, 8)]}.
♪ melody{start:(15, 1, f), relative:(f, 0, 4), run:[0, (0, 8), -3, (-3, 8)]}.
♪ melody{start:(16, 1, f), relative:(f, 0, 8), run:[0, -3, -5, -7]}.

