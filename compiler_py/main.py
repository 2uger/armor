from recursive_descent_parser import parse_program
from table_driven_parser import parser
from tokens import create_tokens


def main():
    tokens = create_tokens()
    for t in tokens:
        print(t)
    parser(tokens)

if __name__ == '__main__':
    main()
