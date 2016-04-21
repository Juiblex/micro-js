/* Cf https://github.com/traitsjs/traits.js */
/* Essentially, traits are property dictionaries, with additional fields
 * to track conflicts when composing and field requiredness */

var Trait = require('traits.js');

var TPrice = function(price) {
	return Trait({
		price: function() {
			return price;
		},
		setPrice: function(p) {
			price = p;
		}
	});
};

var TSpeed = function(speed) {
	return Trait({
		speed: function() {
			return speed;
		},
		setSpeed: function(s) {
			speed = s;
		}
	});
};r

var TName = function(name) {
/*var name = name; */ // Add this to protect against calls without the argument
	return Trait({
		name: function() {
			return name;
		},
		setName: function(n) {
			name = n;
		}
	});
};	

var TVehicle = Trait({
	speed: Trait.required,
	price: Trait.required,
	name: Trait.required,
	print: function() {
		console.log(this.name() + ' ' + this.price() + ' ' + this.speed());
	}
});

//var myCar = Trait.create(Object.prototype, TVehicle); error: missing required props

var myCar = Trait.create(Object.prototype,
							Trait.compose(TVehicle, TPrice(1000), TSpeed(100), TName('foo')));
myCar.print();
