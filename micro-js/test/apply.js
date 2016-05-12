f = function(o, f) {
	return o[f](o, f);
};

f([f:f], "f");
