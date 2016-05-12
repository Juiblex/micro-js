// Flow checks this without errors but there are
// It's used a lot in jQuery

PCar = [];

newCar = function() {
	this.name = "Ferrari";
	this.speed = 300;
	this.price = 500000;
	this.__proto__ = PCar;
};

PCar.setName = function(name) {
	this.name = name;
	return this;
};

PCar.setSpeed = function(speed) {
	this.speed = speed;
	return this;
};

// Would be proven wrong if we ensured that the returned object has
// certain fields (for example, the same as the this object, here)

PCar.setPrice = function(price) {
	this.price = price;
	if (true) {
		return this;
	} else {
		return [];
	}
};

PCar.print = function() {
	print(this.name);
	print(this.speed);
	print(this.price);
};

renault = [init: newCar];
renault.init();

// You could have a relationship between the fields of an object
// that is lost and restored though the chained calls
// like f(speed, price) = 0
renault.setName("Renault").setSpeed(150).setPrice(30000).print();
