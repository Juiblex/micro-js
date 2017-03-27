//Where there is no explicit 'this' parameter, this:any is implied

// x:int, y:int -> null ('new' has special semantics)
function TwoDPoint(x, y) { //init code
	this.x = x;
	this.y = y;
}

// this:{x:int, y:int} -> ret:int
TwoDPoint.prototype.norm = function() { //app code
	return this.x*this.x + this.y*this.y;
}

// this:{x:int, y:int} -> ret:int
TwoDPoint.prototype.foo = function() {
	return this.x + this.y;
}

// x:int, y:int, z:int -> null
function ThreeDPoint(x, y, z) { //init code
	TwoDPoint.apply(this, [x, y]);
	this.z = z;
}

ThreeDPoint.prototype = new TwoDPoint(0, 0);
delete ThreeDPoint.prototype.x;
delete ThreeDPoint.prototype.y;

//this:{x:int, y:int, z:int} -> int
ThreeDPoint.prototype.norm = function() {
	return this.x*this.x + this.y*this.y + this.z*this.z;
}

// this:any -> ret:null
function main() { //app code

var twoD = new TwoDPoint(3, 2);
console.log(twoD.norm());

var threeD = new ThreeDPoint(1, 2, 3);
console.log(threeD.norm());
console.log(threeD.foo());

}

main();
