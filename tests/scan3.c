#include <stdio.h>

int question(int i){

    char str [10];
    int a;
    char c;

    if (i == 0){
        printf ("Enter a str: ");
        scanf ("%s",str);  
        printf ("entered: %s\n",str);  
    }else if (i == 1){
        printf ("Enter a char: ");
        scanf ("%c", &c );
        printf ("entered: %c\n", c);
    }else{
        printf ("Enter your age: \n");
        scanf ("%d\n",&a);
        printf ("entered: %d\n",a);
    }

    return i * 3;

}


int main ()
{
    int i;
    int sum;

    i = 0;
    sum = -58;

    while (i < 3){
        sum = sum + question(i);
        i = i + 1;
    }

    return sum;
}
