PCar = [];

newCar = function() {
	this.name = "";
	this.speed = 0;
	this.price = 0;
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

myCar = [init: newCar];
myCar.init();

// You could have a relationship between the fields of an object
// that is lost and restored though the chained calls
// like f(speed, price) = 0
myCar.setName("myCar").setSpeed(150).setPrice(30000).print();
