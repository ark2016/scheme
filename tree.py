from os import walk, sep
from os.path import basename, isdir
from sys import argv

space = '    '
branch = '  ┃'
turn = '  ├─── '
use = 'Usage: %s [-h] [-f] [-o] [PATH] [FILE]\n\
Print tree structure. \n\
Options: \n\
-h, --help	Print help info \n\
-f	 Print files as well as directories \n\
-o 	 write tree to input file \n\
PATH		Path to process\n'

def tree(startpath, print_files=False):
    for root, subdirs, files in walk(startpath):
        level = root.replace(startpath, '').count(sep)
        indent = branch * (level - 1) + turn
        print("%s%s/" % (indent, basename(root)))
        subindent = branch * level + turn
        if print_files:
            for f in files:
                print("%s%s" % (subindent, f))

def print_tree_in_file(startpath, way, print_files=False):
    f = open(way, 'w')
    for root, subdirs, files in walk(startpath):
        level = root.replace(startpath, '').count(sep)
        indent = branch * (level - 1) + turn
        f.write(indent + basename(root) + '\n')
        subindent = branch * level + turn
        if print_files:
            for f in files:
                f.write(subindent + f + '\n')
    f.close()

def main():
    path = '.'
    if ('-h' or '--help') in argv:
        print(use)
    elif len(argv) == 1:
        tree(path)
    elif len(argv) == 2 and argv[1] == '-f':
        tree(path, True)
    elif len(argv) == 3 and argv[1] == '-o':
        print_tree_in_file(path, argv[-1])
    elif len(argv) == 4 and (argv[1] == '-o' and argv[2] == '-f' or argv[1] == '-f' and argv[2] == '-o'):
        print_tree_in_file(path, argv[-1], True)
    elif len(argv) == 2:
        path = argv[1]
        if isdir(path):
            tree(path)
        else:
            print('ERROR: \'' + path + '\' is not a directory')
    elif len(argv) == 3 and argv[1] == '-f':
        path = argv[2]
        if isdir(path):
            tree(path, True)
        else:
            print('ERROR: \'' + path + '\' is not a directory')
    else:
        print('ERROR: Wrong parameters')
        print(use)


if __name__ == '__main__':
    main()
