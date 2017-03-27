function Foo() { //empty object with prototype Foo.prototype
	this.x = 0;
};

Foo.prototype.y = 1;

function writeY(o:{y:number}, k:number) {
	o.y = k;
}

var o = new Foo();
writeY(o, 42); // typechecks with Flow, but creates a new field in o
