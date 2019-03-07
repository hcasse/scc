int x, y;

int abs(int i) {
	if(i < 0)
		i = -i;
	return i;
}

void imm() {
	x = y | 255;
	x = y & 255;
	x = y ^ 255;
	x = y << 4;
	x = y >> 4;
}

void muldiv_pow() {
	x = y * 32;
	x = y * 5;
	x = y / 128;
	x = y / 3;
	x = y % 64;
	x = y % 3;
}

void trivial() {
	x = y + 0;
	y = y - 0;
	x = y | 0;
	x = y * 1;
	x = y / 1;
}

void null() {
	x = y & 0;
	x = y * 0;
}

