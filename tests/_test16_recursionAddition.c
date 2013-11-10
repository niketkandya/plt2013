int fun(int a) {
	if(a > 0){
	return (a + fun( a -1));
	}
	else {
	return 0;
	}
}
int main() {
	return fun(3);
}
