

float fabs(float x) {
	if(x < 0)
		x = -x;
	return x;
}

float fabs2(float x) {
	if(x >= 0)
		return x;
	else
		return -x;
}

void sum() {
	int i = 0, s = 0;
	while(i < 100) {
		s = s + i;
		i = i + 1;
	}
}

void sum2() {
	int i, s = 0;
	for(i = 0; i < 100; i++)
		s = s + i;
}

void pairsum() {
	int i, s = 0;
	for(i = 0; i < 100; i++)
		if(i % 2 == 0)
			s = s + i;
}

