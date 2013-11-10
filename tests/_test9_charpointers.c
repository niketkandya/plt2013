char main()
{
	char *a;
	char b;
	b = 'a';
	a = &b;
	*a = 'c';
	return b;
}
