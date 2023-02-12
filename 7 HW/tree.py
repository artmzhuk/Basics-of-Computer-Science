import os



def tree(directory, depth=-1, show_files=False):

    if depth == 0:

        return



    try:

        files = os.listdir(directory)

    except PermissionError:

        print(f"Permission denied: {directory}")

        return



    for file in files:

        path = os.path.join(directory, file)

        print("|   " * depth + "|-- " + file)

        if os.path.isdir(path):

            if show_files:

                tree(path, depth + 1, show_files)

            else:

                tree(path, depth + 1)



if __name__ == "__main__":

    import argparse



    parser = argparse.ArgumentParser(description="Tree utility in Python")

    parser.add_argument("directory", help="Directory to display tree for")

    parser.add_argument("-d", help="Only show directories", action="store_true")

    parser.add_argument("-o", help="Don't show files", action="store_true")



    args = parser.parse_args()

    tree(args.directory, show_files=not args.d, depth=not args.o)