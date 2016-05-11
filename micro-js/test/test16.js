x = [foo:0];
x.f =  function() {
	this.foo = this.foo + 1;
};

while (x.foo != 10) {
	x.f();
	print(x.foo);
}

