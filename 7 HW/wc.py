import sys



def count_lines(file):

    return len(file.readlines())



def count_words(file):

    return len(file.read().split())



def count_chars(file):

    return len(file.read())



def wc(file, options):

    if 'l' in options:

        print(count_lines(file), end=' ')

    if 'w' in options:

        print(count_words(file), end=' ')

    if 'm' in options:

        print(count_chars(file))
        
        
    if 'c' in options:

        print(count_chars(file))

    print(file.name)


if __name__ == '__main__':

    options = sys.argv[1]

    files = sys.argv[2:]

    if not files:

        wc(sys.stdin, options)

    else:

        for file in files:

            try:

                with open(file) as f:

                    wc(f, options)

            except IOError as e:

                print(f"wc: {file}: {e}", file=sys.stderr)