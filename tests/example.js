function extend(target, src) {
	var prop;
	for (prop in src)
		if (src.hasOwnProperty(prop))
			target[prop] = src[prop];
};

var Foo = function() {
	this.foo = 42;
};

var PBar = {
	_bar: null, //default value stored in the prototype
	bar: function() {
		return this._bar;
	},
	setBar: function(b) {
		this._bar = b; //new variable in the object, not the prototype one
	}
};

extend(Foo.prototype, PBar);

var foo = new Foo();
console.log(foo.bar());
foo.setBar(42);
console.log(foo.bar()); // 42
