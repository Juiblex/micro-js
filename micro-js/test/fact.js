fact = 1;
fact = function(x) {
	if (x == 1) {
		return 1;
	} else {
		return x * fact(x-1);
	}
};

print(fact(5));
