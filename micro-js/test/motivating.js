
// null -> null
Car = function() { // init code
	this.name = "";
	this.speed = 0;
	this.price = 0;
};

/* (this:{name:any}, name:any) -> ret:this */
Car.prototype.setName = function(name) { // app code
	this.name = name;
	return this;
};

Car.prototype.setSpeed = function(speed) { // app code
	this.speed = speed;
	return this;
};

/* this:{name:any, speed:any, price:any} -> null */
Car.prototype.print = function() { // app code
	console.log(this.name);
	console.log(this.speed);
};

/* A call to "new" creates a new object in the symbolic heap, and runs the	*
 * initialization function on it. Then, the return value (either "this" or	*
 * an explicit value) is typeified, and summarized into the clean heap.			*
 * Here, the myCar object gets type {name:string, speed:int, price:int}			*
 * with the refinement that its prototype is "Car.prototype".								*/

/* type of myCar: {name:string, speed:int} | Car.prototype */

myCar = new Car();
/* These calls check, because we can typeify "Car.prototype" to							*
 * {setName: ..., setSpeed: ..., setPrice:..., print:...}										*/
myCar.setName("myCar").setSpeed(150).setPrice(30000).print();

/* (car1:{}|Car, car2:{name, speed, price}) -> null								
/* If we had given the different type for car1: {setThing:...}, the call
 * would also have checked.																									*/
var matchCar = function(car1, car2) { //app code
	car1.setName(car2.name).setSpeed(car2.speed).setPrice(car2.price);
}

otherCar = new Car();
matchCar(otherCar, myCar);
otherCar.print();
