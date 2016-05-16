makeCounter = function() {
	x = 0;
	return [incr: function() { x = x+1; }, decr: function() { x = x-1; },
					val: function() { return x; }];
};

c = makeCounter();
c.incr();
print(c.val());
c.decr();
print(c.val());
