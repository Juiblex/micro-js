x = [foo: 2+2, y: (function(x) { return 2*x; })(21)];
sum = function(x, y) {
	return x + y;
};
y = [a: sum(x.foo, x.y)];
print(y);
