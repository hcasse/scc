int i, j;
char c;
float x;

void neg() { i = -i; }
void inv() { i = ~i; }

void add() { i = i + j; }
void sub() { i = i - j; }
void mul() { i = i * j; }
void div() { i = i / j; }
void mod() { i = i % j; }
void or() { i = i | j; }
void and() { i = i & j; }
void xor() { i = i ^ j; }
void shl() { i = i << j; }
void shr() { i = i >> j; }

void itof() { x = (float)i; }
void ftoi() { i = (int)x; }
void itoc() { c = (char)i; }
void ctoi() { i = (int)c; }
void ftoc() { c = (char)x; }
void ctof() { x = (float)c; }
