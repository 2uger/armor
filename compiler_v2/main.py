import sys

import asm
from context import Context
from parser import parse
from tokens import create_tokens, tokens
from symbol_table import SymbolTable


def main():
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        print('Provide file name')
        return

    # Tokenize stage
    print('START TOKENIZE STAGE')
    create_tokens(file_name)
    # Parser stage
    print('START PARSER STAGE')
    root = parse(0)

    symbol_table = SymbolTable()
    code = []
    ctx = Context()
    print('START GENERATE ASM')
    try:
        root.make_asm(symbol_table, code, ctx)
    except Exception as e:
        print(e.args[0])
        return

    with open('a.out', 'w') as f:
        for c in code:
            if not isinstance(c, asm.Lable):
                code = '    ' + str(c)
            else:
                code = str(c)
            f.write(code)
            f.write('\n')

if __name__ == '__main__':
    main()
