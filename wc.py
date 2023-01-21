import os
import sys
import getopt

myhelp = "Usage: \n\
%s [options]... [file]...\n\
Options:\n\
-c,--bytes: print the byte counts.\n\
-l,--lines: print the newline counts.\n\
-w,--words: print the word counts.\n\
-m,--chars: print the character counts.\n\
--help: print this help and exits.\n" % sys.argv[0]


def sort_opts(opt):
    opt_order = {'-l': 1, '-w': 2, '-m': 3, '-c': 4}
    return opt_order[opt], opt


def count_words_lines_bytes_stdin(filename, opts):
    wordcount = 0
    countlines = 0
    charcount = 0
    for line in filename:
        charcount += len(line)
        line = line.split()
        wordcount += len(line)
        countlines += 1
    countbytes = charcount
    x = []
    filename = ''
    if opts:
        results = {'-c': countbytes, '-l': countlines, '-w': wordcount, '-m': charcount}
        x = [str(results[i]) for i in opts]
        x.append(filename)
    else:
        for i in [countlines, wordcount, countbytes, filename]:
            x.append(str(i))
    return x


def count_words_lines_bytes(filename, opts):
    if len(opts) == 0:
        opts = ['-l', '-w', '-m']
    if os.path.isfile(filename):
        countbytes = os.stat(filename).st_size
        try:
            myfile = open(filename)
        except Exception as e:
            print(e)
            return 0
    elif os.path.isdir(filename):
        print("%s: Is a directory" % filename)
        p = [str(0) for i in opts]
        p.append(filename)
        return p
    else:
        print("%s: No such file or directory" % filename)
        return 0
    if len(opts) == 1 and opts[0] == '-c': return [str(countbytes), filename]
    if len(opts) == 1 and opts[0] == '-m': return [str(countbytes), filename]
    if len(opts) == 2 and opts[0] == '-m' and opts[1] == '-c':
        return [str(countbytes), str(countbytes), filename]
    wordcount = 0
    countlines = 0
    for line in myfile:
        line = line.split()
        wordcount += len(line)
        countlines += 1
    x = []
    results = {'-c': countbytes, '-l': countlines, '-w': wordcount, '-m': countbytes}
    x = [str(results[i]) for i in opts]
    x.append(filename)
    return x


def report(results, n):
    if n == 0:
        n = 3
    justification = []
    for i in range(n + 1):
        p = [j[i] for j in results]
        q = max([len(k) for k in p])
        justification.append(q)
    rj = justification.pop()
    myformat = ' '.join(['{' + str(k) + ':>' + str(v) + '}' for (k, v) in enumerate(justification)])
    myformat += ' {' + str(n) + ':<' + str(rj) + '}'
    for i in results:
        print(myformat.format(*i))


def total(results, n):
    if n == 0:
        n = 3
    total = []
    for i in range(n):
        total.append(str(sum([int(k[i]) for k in results])))

    total.append('total')
    results.append(total)
    return results


if __name__ == '__main__':
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'cwlm',
                                   ['bytes', 'words', 'lines', 'chars', 'version', 'help', 'files0-from='])
    except getopt.GetoptError as e:
        print("Bad arguments. Check help below.")
        print(myhelp)
    newopts = {}
    results = []
    opts = dict(opts)
    for o in opts:
        if o in ('-c', '--bytes'):
            newopts.setdefault('-c')
        elif o in ('-w', '--words'):
            newopts.setdefault('-w')
        elif o in ('-l', '--lines'):
            newopts.setdefault('-l')
        elif o in ('-m', '--chars'):
            newopts.setdefault('-m')
        elif o in ('--help'):
            print(myhelp)
    if len(args) == 0:
        results = [count_words_lines_bytes_stdin(sys.stdin, sorted(newopts, key=sort_opts))]
    else:
        for arg in args:
            x = count_words_lines_bytes(arg, sorted(newopts, key=sort_opts))
            if x == 0:
                pass
            else:
                results.append(x)
    if len(args) > 1: 
        total(results, len(newopts))
    if results:
        report(results, len(newopts))
