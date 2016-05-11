k = 0;
f = function() {
	if (k == 0) {
		k = k+1;
		return "foo";
	} else {
		k = k-1;
		return "bar";
	}

};
x = [foo: 1, bar: 2, g: f];

i = 0;
while (i < 10) {
	i = i + 1;
	print(x[x["g"]()]);
}
