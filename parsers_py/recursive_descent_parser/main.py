import sys

from recursive_descent_parser import parse
from tokens import create_tokens


def main():
    file_name = 'm1.io'
    #if len(sys.argv) > 1:
    #    file_name = sys.argv[1]
    #else:
    #    print('Provide file name');
    #    return
    tokens = create_tokens(file_name)
    for t in tokens:
        print(t)
    parse(tokens)

if __name__ == '__main__':
    main()
