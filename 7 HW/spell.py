import sys

def spell_check(dictionary, text):
    with open(dictionary, 'r') as f:
        words = set(line.strip() for line in f)

    errors = []
    for i, line in enumerate(open(text, 'r')):
        for j, word in enumerate(line.strip().split()):
            if word not in words:
                errors.append((i+1, j+1, word))

    return errors

if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: spell_check.py dictionary_file text_file")
        sys.exit(1)

    dictionary, text = sys.argv[1], sys.argv[2]
    errors = spell_check(dictionary, text)
    for error in errors:
        print("{}, {} {}".format(error[0], error[1], error[2]))