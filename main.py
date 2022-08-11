import argparse

import asm
import utils
from context import Context
from parser import parse
from tokens import create_tokens, tokens
from symbol_table import SymbolTable


def main():
    args_parser = argparse.ArgumentParser()
    args_parser.add_argument('file_in', type=str, help='name of input file')
    args_parser.add_argument('-o', type=str, default='out.asm', help='name of output file')

    compiler_args = args_parser.parse_args()
    file_in = compiler_args.file_in
    file_out = compiler_args.o

    try:
        # Tokenize stage
        utils.messages.append('TOKENIZE STAGE')
        create_tokens(file_in)
        # Parser stage
        utils.messages.append('PARSER STAGE')
        root = parse(0)

        symbol_table = SymbolTable()
        code = []
        ctx = Context()
        utils.messages.append('GENERATING ASM')
        root.make_asm(symbol_table, code, ctx)
    except Exception as e:
        raise e
        RED_COLOR = '\033[91m'
        RESET = '\033[0m'
        print('ERROR: ', RED_COLOR + e.args[0] + RESET)
        return

    with open(file_out, 'w') as f:
        """
        First elements of code is Data
        it's global variables counted in compile time - move them to .data section
        After them just an often code - move it to .text section
        """
        data = []
        text = []
        for c in code:
            if isinstance(c, asm.Data):
                data.append(c)
            else:
                text.append(c)
        f.write('Section: .data:\n')
        for d in sorted(data, key=lambda x: x.mem_binding):
            f.write(f'{hex(d.mem_binding)}\t{hex(d.value)}\t // {d.name}\n')
        f.write('\nSection: .text:\n')
        for t in text:
            if not isinstance(t, asm.Lable):
                cmd = '    ' + str(t)
            else:
                cmd = str(t)
            f.write(cmd + '\n')
    
    for msg in utils.messages:
        print(msg)

if __name__ == '__main__':
    main()
