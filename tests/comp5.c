int i, s;

void _while() {
	i = 0;
	s = 0;
	while(i < 10) {
		s = s + i;
		i = i + 1;
	}
}

void _dowhile() {
	i = 0;
	s = 0;
	do {
		s = s + i;
		i = i + 1;
	} while(i < 10);
}
