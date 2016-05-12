x = [f:0];
x.f = function() { print("Hello"); return this.f; };

x.f()();
