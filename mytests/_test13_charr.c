char fun(char *b) {
	*b = 'a';
}
char main()
{
	char b[4];
	b[3] = 'b';
	fun(&b[3]);
	return b[3];
}
