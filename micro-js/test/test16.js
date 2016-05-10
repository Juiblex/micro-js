x = [f: function() {
	this.foo = this.foo + 1;
}, foo:0];

while (x.foo != 100) {
	x.f();
}

print(x.foo);
