void fun(int *b) {
	*b = 50;
}
char main()
{
	int b[4];
	b[3] = 30;
	fun(&b[3]);
	return b[3];
}
