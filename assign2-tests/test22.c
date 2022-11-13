int sub(int a, int b) {return a - b;}
int add(int a, int b) {
	int c = sub(a, b);
	int d = sub(b, a);	
	return a + b + add(c, d);
}
int (*clever())(int, int) {
	if (add(3, 4))
		return add;
	return sub;
}
int moo(int op, int op2, int op3) {
	int x = 0;
	int y = 0;
	if (op == 1) {
		x = 1;
		y = 1;
	}

	add(x, y);

	return 0;
}