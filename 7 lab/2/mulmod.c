#include <stdio.h>

int main(int argc, char **argv)
{
    unsigned long long a, b, m, b2, result;
    unsigned long long i = 0; // i-номер двоичного разряда;
    int arr[64];
    //printf("input\n");
    scanf("%llu %llu %llu", &a, &b, &m);
    b2 = b;
    while (b2 > 0) 
    {
        arr[i] = b2 % 2;
        b2 /= 2;
        ++i;
    }
    result = a * arr[i - 1];
    i--;

    for (; i >= 1; --i)
    {
        result = (result % m) * 2;
        result = (result % m) + (a % m) * (arr[i-1] % m);
    }
    result = result % m;
    printf("%llu\n", result);
    return 0;
}
// a 9223372036854775807 (2^63)
// b 9223372036854775807
// m 5555555555
// 3026275444
