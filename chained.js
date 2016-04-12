// Flow checks this without errors but there is
// It's used a lot in jQuery

var Car = function() {
	this.name = "Ferrari";
	this.speed = 300;
	this.price = 500000;
}

Car.prototype.setName = function(name) {
	this.name = name;
	return this;
}

Car.prototype.setSpeed = function(speed) {
	this.speed = speed;
	return this;
}

// Would be proven wrong if we ensured that the returned object has
// certain fields (for example, the same as the this object, here)

Car.prototype.setPrice = function(price) {
	this.price = price;
	if (Math.random() < 0.5)
		return this;
	else
		return {};
}

Car.prototype.print = function() {
	console.log('Name: ' + this.name + '\n' +
							'Speed: ' + this.speed + '\n' +
							'Price: ' + this.price + '\n');
}

var renault = new Car();

// You could have a relationship between the fields of an object
// that is lost and restored though the chained calls
// like f(speed, price) = 0
renault.setName('Renault').setSpeed(150).setPrice(30000).print();
