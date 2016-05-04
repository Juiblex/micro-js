/* JS implementation of the stuff described in:															*
 * Adding State and Visibility Control to Traits using Lexical Nesting			*
 * Van Cutsem et al., ECOOP 09																							*/

/* For now I'm sticking to their implementation so no prototype stuff, I'll *
 * add that later if it makes an interesting difference.										*/

var Counter = function(val) {

	this.count = function() {
		return val;
	};

	this.inc = function() {
		val = val + 1;
	};

	this.dec = function() {
		val = val - 1;
	};
};

function isStrictlyPositive(x) {
	return x > 0;
}

var PositiveCounter = function(val) {
	Counter.call(this, val);
	this.dec = (function() { /* Black magic */
		var super_dec = this.dec;
		return function() { 
				if (isStrictlyPositive(this.count()))
					super_dec();
				else
					console.log("Trying to decrease under 0");
			};
	}).call(this);
};

