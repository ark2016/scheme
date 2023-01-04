import random

def print_lines(length, num):
    for i in range(num):
        line = ''
        for j in range(length):
            line += chr(random.randint(33,126))
        print(line)
