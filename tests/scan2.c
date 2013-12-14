#include <stdio.h>

int func (int a){
    char str [80];
    int i;
    printf ("Enter your family name: ");
    scanf ("%s",str);  
    printf ("Enter your age: ");
    scanf ("%d",&i);
    printf ("Mr. %s , %d years old.\n",str,i);
    printf ("Enter a hexadecimal number: ");
    scanf ("%x",&i);
    printf ("You have entered %#x (%d).\n",i,i);

    return i;
}

int main ()
{
    int i;
    int sum;

    i = 0;
    sum = 0;

    while (i < 3){
        sum = sum + func(i);
        i = i + 1;
    }

 return sum;
}
