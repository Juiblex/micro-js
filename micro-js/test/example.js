extend = function(X, Y) {
	for (y in Y) {
		X[y] = Y[y];
	}
};

PFoo = [];

Foo = function() {
	this.foo = 42;
	this.__proto__ = PFoo;
};

PBar = [
	_bar: 0,
	bar: function() {
		return this._bar;
	},
	setBar: function(b) {
		this._bar = b;
	}
];

extend(PFoo, PBar);

foo = [init: Foo]; // instead of new Foo()
foo.init();

print(foo.bar());
foo.setBar("hello");
print(foo.bar()); // works
