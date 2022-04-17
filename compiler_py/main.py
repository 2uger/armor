from parser import parse_program
from tokens import create_tokens


def main():
    tokens = create_tokens()
    for t in tokens:
        print(t)
    parse_program(tokens)

if __name__ == '__main__':
    main()
