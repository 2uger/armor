import sys

from context import Context
from parser import parse
from tokens import create_tokens, tokens
from symbol_table import SymbolTable


def main():
    file_name = 'm1.io'
    #if len(sys.argv) > 1:
    #    file_name = sys.argv[1]
    #else:
    #    print('Provide file name');
    #    return
    create_tokens(file_name)
    for t in tokens:
        print(t)
    root = parse(0)

    symbol_table = SymbolTable()
    code = []
    ctx = Context()
    try:
        root.make_asm(symbol_table, code, ctx)
    except Exception as e:
        print(e.args[0])
        return
    for c in code:
        print(c)

if __name__ == '__main__':
    main()
