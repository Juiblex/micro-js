function Foo() { // init
	this.foo = "hello";
	this.bar = 42;
}

// type: unit -> null 
function f() { // app
	var x = new Foo();
	g(x);
	console.log(x.foo + x.bar);
}

function g(x) { // init
	var y = x;
	y.bar = 0; // should check
	y.bar = " world"; // shouldn't check
}


f();
