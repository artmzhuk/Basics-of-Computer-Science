from secrets import choice
from random import sample
from string import digits, ascii_letters, punctuation

def randstr(length, count):
	if (type(length) != int or type(count) != int):
		return print("Arguments should be integer")

	if (count <= 0 or length <= 0):
		return print("Arguments should be positive")

	alphabet = digits + ascii_letters + punctuation

	strings = [""]*count
	for i in range(count):
		strings[i] = ''.join(choice(alphabet) for j in range(length))

	return strings
