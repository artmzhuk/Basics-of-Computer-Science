#!/usr/bin/env python3


import sys
from mystrings import randstr

if __name__ == '__main__':
    length = int(sys.argv[1])
    strings = int(sys.argv[2])

    print(*randstr(length, strings), sep="\n")
