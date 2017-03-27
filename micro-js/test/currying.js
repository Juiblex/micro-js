function f1(x, y) {
	;
}

is equivalent to

function f2(x) {
	return function g(y) {
		(x is in scope)
	}
}

modulo calling f2(x)(y) instead of f1(x, y)
