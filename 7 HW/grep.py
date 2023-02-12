import re

import sys



def grep(pattern, files, ignore_case=False, max_matches=None, line_numbers=False):

    """

    Search for a pattern in the given files and print the matching lines.

    :param pattern: The regular expression to search for.

    :param files: A list of file paths to search in.

    :param ignore_case: A flag indicating whether to ignore case when searching.

    :param max_matches: The maximum number of matches to print.

    :param line_numbers: A flag indicating whether to print line numbers.

    """

    if ignore_case:

        pattern = re.compile(pattern, re.IGNORECASE)

    else:

        pattern = re.compile(pattern)



    matches = 0

    for file in files:

        try:

            with open(file, 'r') as f:

                for i, line in enumerate(f):

                    if pattern.search(line):

                        if line_numbers:

                            print(f'{file}:{i+1}:{line}', end='')

                        else:

                            print(line, end='')

                        matches += 1

                        if max_matches is not None and matches >= max_matches:

                            return

        except Exception as e:

            sys.stderr.write(f'Error reading file {file}: {e}\n')



if __name__ == '__main__':

    import argparse



    parser = argparse.ArgumentParser()

    parser.add_argument('pattern', type=str)

    parser.add_argument('files', nargs='*', type=str)

    parser.add_argument('-i', action='store_true')

    parser.add_argument('-m', type=int)

    parser.add_argument('-n', action='store_true')

    args = parser.parse_args()



    grep(args.pattern, args.files, args.i, args.m, args.n)