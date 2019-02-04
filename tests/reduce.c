
void main() {
	int i = 0;
	int j = 100 - i;
	int s = 0;
	
	while(i > j) {
		s = s + (j - i) & ((4096 >> 2) - 1);
		i++;
		j = j - (1 + 2 + 3 + 4 + 5) / 15;
	}
	
}
