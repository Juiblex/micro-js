mk_const = function(x) {
	return function() { return x; };
};

_3 = mk_const(3);
print(_3());
_yolo = mk_const("yolo");
print(_yolo());
