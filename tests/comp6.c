int i, j;

void _not() {
	if(!(i <= 0))
		i = -1;
}

void _and() {
	if(0 <= i && i < 10)
		j = i;
	else
		j = -1;
}

void _or() {
	if(i <= 0 || 10 <= i)
		j = -1;
	else
		j = i;
}
