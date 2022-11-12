int add(int a, int b) {return a + b;}
int moo(int op, int op2, int op3) {
	int x = 0;
	int y = 0;
	if (op == 1) {
		x = 1;
		y = 1;
	} else if (op == 2) {
		x = 2;
		y = 2;
	}

	if (op2 == 1) {
		y = 4;
	}

	if (op3 == 1) {
		x = 3;
	}

	add(x, y);

	return 0;
}