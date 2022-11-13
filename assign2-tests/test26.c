int (*foo())(int) {
	foo();
	return 0;
}

int (*f2())(int);
int (*f3())(int);
int (*f1())(int) {
	return f2();
}
int (*f2())(int) {
	return f3();
}
int (*f3())(int) {
	return f1();
}