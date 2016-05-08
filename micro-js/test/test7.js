makeCounter = function() {
	x = 0;
	return [incr: function() { x = x+1; }, decr: function() { x = x-1; }];
};

c = makeCounter();
c.incr();
c.decr();
