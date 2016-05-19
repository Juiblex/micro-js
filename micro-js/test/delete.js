x = [foo:1, bar:2];
delete x.foo;
delete x[(function() { return "bar"; })()];
print(x);
print([]);
