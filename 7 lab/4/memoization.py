#!/usr/bin/env python3


def memoize(func):
    memo = {}  # словарь вычисленных ранее значений

    def memorized(*args):  # вспомогательная функция
        if args in memo:  # если нашелся результат по ключу-аргументам, то возвращаем его
            return memo[args]
        else:
            memo[args] = func(*args)  # иначе вычисляем и возвращаем
            return memo[args]

    return memorized


def fib(n):
    if n < 2:
        return n
    return fib(n - 2) + fib(n - 1)


fib = memoize(fib)
print(fib(10))
print(fib(15))
