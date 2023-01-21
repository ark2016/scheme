import re
from sys import argv


def i_grep(word, file):
    f = open(file, 'r')
    for line in f:
        if re.search(word.lower(), line.lower()):
            print(line)
    f.close()


def m_grep():
    f = open(argv[-1], 'r')
    counter = 0
    for line in f:
        if re.search(argv[-2]):
            print(line)
            counter += 1
        if counter >= argv[-3]:
            break
    f.close()


def n_grep():
    f = open(argv[-1], 'r')
    counter = 0
    for line in f:
        if re.search(argv[-2]):
            counter += 1
            print("%d%s" % (counter, line))
    f.close()


def grep(word, file):
    f = open(file, 'r')
    for line in f:
        if re.search(word):
            print(line)
    f.close()


def main():
    if len(argv) == 3:
        grep(argv[1], argv[2])
    elif len(argv) == 4 and argv[1] == '-e':
        grep(argv[1], argv[2])
    elif len(argv) == 4 and argv[1] == '-i':
        i_grep()
    elif len(argv) == 5 and argv[1] == '-n':
        n_grep()
    elif len(argv) == 5 and argv[1] == '-m':
        m_grep()
    else:
        print("ERROR: wrong input")


if __name__ == '__main__':
    main()
