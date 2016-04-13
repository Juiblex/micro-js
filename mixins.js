/* Mostly adapted from
 * http://raganwald.com/2014/04/10/mixins-forwarding-delegation.html */

// Basic version, doesn't shield _price (like jQuery.extend())

function extend(target, src) { // extend target with properties from src
	var prop;
	for (prop in src) {
		if (src.hasOwnProperty(prop)) {
				// check that it's from src and not its prototype
				// (we could also choose to copy prototype fields)
			target[prop] = src[prop]; // overwrites existing properties from target
		}
	}
}

var Car = function() {
	this.wheels = 4;
}


var Price = {
	price: function() {
		return this._price;
	},
	
	setPrice: function(newPrice) {
		this._price = newPrice;
	}
};

extend(Car.prototype, Price);
var car = new Car();
car.setPrice(1000);
console.log(car.price());
// car._price = 1000

// Functional version, shields _name
function addName(target) {
	var _name =  "";

	target.name = function() {
		return _name;
	};

	target.setName = function(newName) {
		_name = newName;
	};
};

addName(Car.prototype);

var ferrari = new Car();
ferrari.setName("ferrari");
console.log(ferrari.name());
// ferrari._name = undefined

// Object version with shielding

function extendShield(target, src) {
	var hidden = {};
	var prop;
	for (prop in src) {
		if (src.hasOwnProperty(prop) && (typeof src[prop] === 'function')) {
			target[prop] = src[prop].bind(hidden);
		}
		else {
			hidden[prop] = src[prop];
		}
	}
};

var Bike = function() {
	this.wheels = 2;
}

extendShield(Bike.prototype, Price);
var myBike = new Bike();
myBike.setPrice(250);
console.log(myBike.price());
//myBike._price = undefined
