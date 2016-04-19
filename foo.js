var Car = function() {
	this.wheels = 4;
};

var myCar = new Car();

console.log('myCar ' + myCar.wheels);
console.log('proto ' + Car.prototype.wheels); //undefined
console.log('Car ' + Car.wheels); //also undefined
