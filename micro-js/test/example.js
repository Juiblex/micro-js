Foo = function() {
	this.foo = 42;
};

Bar = [
	bar: function() {
		return this._bar;
	},
	setBar: function(b) {
		this._bar = b;
	}
];

foo = [init: Foo];

foo.init();

foo.bar = Bar.bar;
foo.setBar = Bar.setBar;

//print(foo.bar()); raises Undefined field : _bar
foo.setBar("hello");
print(foo.bar()); // works
