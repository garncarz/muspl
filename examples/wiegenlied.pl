extra title('Schlafe, mein Prinzchen, schlaf ein').
extra composer('Bernhard Flies').
extra poet('Friedrich Wilhelm Gotter').

extra scale{root:f, quality:major}.

extra timeSignature(6, 8).
% timeSignature(0, 6, 8). FIXME since when? till when?

:- loadLyrics(v, 'wiegenlied.txt').

m [

melody{start:(1, 1, v), relative:(a, 1, 8),
	pitch:[0, 1, 0, -1, -2, -1, -2], len:[1*6, 2]},
melody{start:(1, 1, g), relative:(f, 1, 8),
	pitch:[0, 0, -1, -2, -1], len:[2, 1*4]},
melody{start:(1, 1, f), relative:(c, 1, 8),
	pitch:[0, 1, 0, -1, -1], len:[1*3, 2, 1]},
melody{start:(1, 1, f), relative:(f, 0, 8),
	pitch:[0, 0, -3, -3], len:[2, 1, 2, 1]},

mel02 = [0, 2, 2, 0, 2, 2],
mel0r = [0, r, r, 0, r, r],
melody{start:(2, 1, f), relative:(a, 0, 8), pitch:mel02},
melody{start:(2, 1, f), relative:(f, 0, 8), pitch:mel0r},

melody{start:(2, 6, v), relative:(f, 1, 8),
	pitch:[0, 0, 3, 3, 3, 4, 5, 4], len:[1*7, 2]},
melody{start:(2, 6, g), relative:(f, 1, 8),
	pitch:[r, -2, 0, 0, -2, 0, 0, 0], len:[1*7, 2]},
melody{start:(3, 1, f), relative:(bes, 0, 8), pitch:mel02},
melody{start:(3, 1, f), relative:(f, 0, 8), pitch:mel0r},

copyBars{from:2, to:4, cond:isStaff(f)},


'6long' = [1*6, [4, 8, 4]],
melody{start:(5, 1, v), relative:(g, 1, 8),
	pitch:[0, -1:1, 0, 0, -1:1, 0, 2], len:'6long'},
melody{start:(5, 1, g), relative:(e, 1, 8),
	pitch:[0, -1:1, 0, 0, -1:1, 0, 2], len:'6long'},
melody{start:(5, 1, f), relative:(c, 0, 4),
	pitch:[0, 7, 0, 7], len:[1, 0.5, 1, 0.5]},
melody{start:(6, 1, f), relative:(c, 0, 8), pitch:[0, 2, 4, 7, 4, 0]},

'7mel' = [0*3, 1, 0, 1, 2],
melody{start:(7, 1, v), relative:(a, 1, 8), pitch:'7mel',
	len:'6long'},
melody{start:(7, 1, g), relative:(f, 1, 8), pitch:'7mel',
	len:'6long'},
melody{start:(7, 1, f), relative:(f, 0, 4),
	pitch:[0, 4, 0, 4], len:[1, 0.5, 1, 0.5]},

copyBars{from:6, to:8, cond:isStaff(f), action:pitchShift(5)},


mel07 = [0, 7, 7, 0, 7, 7],
melody{start:(9, 1, v), relative:(d, 2, 8),
	pitch:[0*4, -1:1, 0, 2], len:'6long'},
melody{start:(9, 1, g), relative:(bes, 1, 8),
	pitch:[0*6, 2], len:'6long'},
melody{start:(9, 1, g), relative:(f, 1, 8),
	pitch:[0*6, 3], len:'6long'},
melody{start:(9, 1, f), relative:(bes, -1, 8), pitch:mel07},

copyBars{from:9, to:10, cond:isStaff(f)},

melody{start:(11, 1, v), relative:(c, 2, 8),
	pitch:[0*4, -1:1, 0, 3], len:'6long'},
melody{start:(11, 1, g), relative:(a, 1, 8),
	pitch:[0*4, -1:1, 0, 2], len:'6long'},
melody{start:(11, 1, g), relative:(f, 1, 8),
	pitch:[0*6, 2], len:'6long'},
melody{start:(11, 1, f), relative:(f, -1, 8), pitch:mel07},

copyBars{from:11, to:12, cond:isStaff(f)},


'13mel' = [0, 1, 0, -1, 0, -1, -2],
melody{start:(13, 1, v), relative:(bes, 1, 8), pitch:'13mel',
	len:'6long'},
melody{start:(13, 1, g), relative:(g, 1, 8), pitch:'13mel',
	len:'6long'},
melody{start:(13, 1, f), relative:(c, 1, 4),
	pitch:[0, 0, 0, 0], len:[1, 0.5, 1, 0.5]},
melody{start:(13, 1, f), relative:(e, 0, 4),
	pitch:[0, 0, 1, 1], len:[1, 0.5, 1, 0.5]},

melody{start:(14, 1, f), relative:(c, 0, 8), pitch:[0, 2, 4, 7, 6, 4]},

melody{start:(15, 1, v), relative:(a, 1, 8),
	pitch:[0, 1, 0, -1, 0, -1, -2], len:'6long'},
melody{start:(15, 1, g), relative:(f, 1, 8),
	pitch:[0, 1, 0, -1, -1, -1, -3], len:'6long'},
melody{start:(16, 1, g), relative:(a, 0, [4, 8, 4]), pitch:[0]},
melody{start:(15, 1, f), relative:(c, 1, 4),
	pitch:[0, 0, -1, -1], len:[1, 0.5, 1, 0.5]},
melody{start:(15, 1, f), relative:(f, 0, 4),
	pitch:[0, 0, -3, -3], len:[1, 0.5, 1, 0.5]},
melody{start:(16, 1, f), relative:(f, 0, 8), pitch:[0, -3, -5, -7]}

].

