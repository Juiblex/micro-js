x = [f: function() { this.foo = 42; }];
y = x.f;
y();
print(x);
