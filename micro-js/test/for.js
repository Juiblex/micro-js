y = [yolo:1];
x = [foo:1, bar:2, __proto__:y];

for (foo in x) {
	print(foo);
}
print(x.yolo);
x.yolo = x.yolo;
print(x.yolo);
