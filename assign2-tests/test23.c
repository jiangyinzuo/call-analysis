
int (*clever())(int, int);
int add(int a, int b) {
	return clever()(a, b) + add(0, 0);
}
int (*clever())(int, int) {
	clever()(1, 1);
	add(3, 4);
	return add;
}