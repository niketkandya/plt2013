void fun(int *b) {
	*b = 45;
}
char main()
{
	int b;
	b = 20;
	fun(&b);
	return b;
}
