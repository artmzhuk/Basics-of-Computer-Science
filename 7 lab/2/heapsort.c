#include <stdio.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

void heapify(void *base, int i, size_t nel, size_t width);
void buildheap(void *base, size_t nel, size_t width);
void hsort(void *base, size_t nel, size_t width,
           int (*compare)(const void *a, const void *b));
void swap(void *base, int a, int b, size_t width);
int compare(const void *a, const void *b);


int main()
{
    int array[6] = {2, 8, 5, 3, 9, 1};
    hsort(array, 6, sizeof(int), *compare);
    //heapify(array, 0, 6, sizeof(int));
    for(int i = 0; i < 6; i++)
    {
        printf("%i ", array[i]);
    }
    printf("\n");
    return 0;
}

void heapify(void *base, int i, size_t nel, size_t width)
{
    int left = 2 * i + 1;
    int right = 2 * i + 2;
    int max = i;

    // char *leftValue = malloc(width);
    // memcpy(leftValue, base + left * width, width);

    if (left < (int)nel && compare(base + left * width, base + max * width) > 0)
        max = left;

    if (right < (int)nel && compare(base + right * width, base + max * width) > 0)
        max = right;

    if (max != i)
    {
        swap(base, i, max, width);

        heapify(base, max, nel, width);
    }
}

void buildheap(void *base, size_t nel, size_t width)
{
    int n = (int)nel;
    for (int i = n / 2 - 1; i >= 0; i--)
    {
        heapify(base, i, nel, width);
    }
}

void hsort(void *base, size_t nel, size_t width,
           int (*compare)(const void *a, const void *b))
{
    int n = (int)nel;
    for (int i = n / 2 - 1; i >= 0; i--)
    {
        heapify(base, i, n, width);
    }
    for (int i = n - 1; i >= 0; i--)
    {
        swap(base, 0, i, width);
        //n =- 1;
        heapify(base, 0, i, width);
    }
}

void swap(void *base, int a, int b, size_t width)
{
    void *tmp = malloc(width);
    memcpy(tmp, base + a * width, width);
    memcpy(base + a * width, base + b * width, width);
    memcpy(base + b * width, tmp, width);
    free(tmp);
}

int compare(const void *a, const void *b)
{
    return (*(int *)a - *(int *)b);
}