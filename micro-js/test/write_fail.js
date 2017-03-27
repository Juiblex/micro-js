function Foo() { //init code
	this.foo = 42;
}

Foo.prototype.bar = 0;

// x:{foo:int, bar:int} -> null
var f = function(x) { //app code
	y = new Foo(); // y is given {foo:int}|Foo.prototype
	x = y; //merged type of y is {foo:int, bar:int} so the write checks
	delete Foo.prototype.bar; //we change Foo.prototype
	console.log(x.bar); //x.bar has now disappeared
}

f({foo:0, bar:42});
