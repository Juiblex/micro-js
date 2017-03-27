/* foo:int -> ret:Foo */
function Foo(foo) { //init code
	this.foo = foo;
}

/* X:any, Y:any -> null */
extend = function(X, Y) {
	for (y in Y) {
		X[y] = Y[y];
	}
};

BarMixin = {
	bar: function() { /* null -> ret:int */
		return this._bar;
	},
	setBar: function(b) { /* b:int -> null */
		this._bar = b;
	}
};

extend(Foo.prototype, BarMixin);

function main() {
	x = new Foo(40);
	x.setBar(2);
	console.log(x.foo + x.bar());
}
