'use strict';

class Point { //can only have methods here, not properties

	constructor(x, y) { 
		this.x = x;
		this.y = y;
		this.z = x+y;
	}

	normSq() {
		return this.x * this.x + this.y * this.y + this.z * this.z;
	}

};

var p = new Point(1, 2);
console.log(p.normSq());
