function extend(target, src) {
	var prop;
	for (prop in src)
		if (src.hasOwnProperty(prop))
			target[prop] = src[prop];
};

var Foo = function() {
	this.foo = 42;
}

var Bar = {
	bar: function() {
		return this._bar;
	},
	setBar: function(b) {
		this._bar = b;
	}
};

extend(Foo.prototype, Bar);

var foo = new Foo();
console.log(foo.bar()); // undefined
foo.setBar(42);
console.log(foo.bar()); // 42
