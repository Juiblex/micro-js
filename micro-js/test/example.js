extend = function(X, Y) {
	for (y in Y) {
		X[y] = Y[y];
	}
};

PFoo = [test:0];

Foo = function() {
	this.foo = 42;
	this.__proto__ = PFoo;
};

PBar = [
	bar: function() {
		return this._bar;
	},
	setBar: function(b) {
		this._bar = b;
	}
];

extend(PFoo, PBar);

foo = [init: Foo];

foo.init();

//print(foo);
//print(PFoo);
//print(PBar);


//print(foo.bar()); raises Undefined field : _bar
foo.setBar("hello");
print(foo.bar()); // works
