int add4(int a) { return a + 4; }
int (*foo())(int) {
	add4(1);
	add4(5);
	return add4;
}

void bar() {
	foo()(1);
	foo()(1);
}