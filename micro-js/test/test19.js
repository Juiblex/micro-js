// invalid in vanilla JS, 'this' is not a valid lhs
x = [f: function() { print(this); this = 42; }];
print(x);
x.f();
print(x);
