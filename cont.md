Contribution Statement
----------------------

We consider the problem of ensuring the safety of Javascript dereferences, by detecting which ones could lead to a run-time error in a program.
Since objects, and their prototypes, are very dynamic and mutable, it is difficult to check whether an object does effectively have a given property at a given point in the program.
By accurately modeling the prototype chain in the heap, as well as tracking relationships between objects with a dependent type system, following the "almost everywhere" idea, we can hope to track the evolution of object properties much more precisely than before.
What follows is a more precise analysis, with less false positive cases of possible error dereferences.
