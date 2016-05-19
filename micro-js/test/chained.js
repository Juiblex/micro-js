/* init code */
PCar = []; //

newCar = function() { // f1
	/* init code */
	this.name = "";
	this.speed = 0;
	this.price = 0;
	this.__proto__ = PCar;
	/* end init */
};

PCar.setName = function(name) { // f2
	// not init code because we're inside a function now
	this.name = name;
	return this;
};

PCar.setSpeed = function(speed) { // f3
	this.speed = speed;
	return this;
};

PCar.setPrice = function(price) { // f4
	this.price = price;
	return this;
};

PCar.print = function() { // f5
	print(this.name);
	print(this.speed);
	print(this.price);
};
/* end init */

myCar = [init: newCar]; // should be just myCar = new newCar();
myCar.init();

myCar.setName("myCar").setSpeed(150).setPrice(30000).print();
