import argparse
import os
from parser import parse

from asm_gen import AsmGen
from context import Context
from ir_gen import IRGen
from symbol_table import NewSymbolTable
from tokens import create_tokens


def main():
    args_parser = argparse.ArgumentParser()
    args_parser.add_argument('file_in', type=str, help='name of input file')
    args_parser.add_argument('-o', type=str, default='out.asm', help='name of output file')

    compiler_args = args_parser.parse_args()
    file_in = compiler_args.file_in
    file_out = compiler_args.o

    if not os.path.exists(file_in):
        print(f'file {file_in} does not exists')
        return

    try:
        # Tokenize stage
        create_tokens(file_in)
        # Parser stage
        root = parse(0)

        new_symbol_table = NewSymbolTable()
        ir_gen = IRGen()
        ctx = Context()

        root.make_ir(new_symbol_table, ir_gen, ctx)

        asm_gen = AsmGen(new_symbol_table, ir_gen)
        asm_gen.make_asm()

    except Exception as e:
        raise e
        RED_COLOR = '\033[91m'
        RESET = '\033[0m'
        print('ERROR: ', RED_COLOR + str(e.args[0]) + RESET)
        return

    with open(file_out, 'w') as f:

        text = []
        for c in asm_gen.cmds:
            text.append(c)
            f.write(c + '\n')
        # f.write('Section: .data:\n')
        # for d in sorted(data, key=lambda x: x.mem_binding):
        #     f.write(f'{hex(d.mem_binding)}\t{hex(d.value)}\t // {d.name}\n')
    
if __name__ == '__main__':
    main()
