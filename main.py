import argparse

from context import Context
from parser import parse
from tokens import create_tokens
from symbol_table import NewSymbolTable
from ir_gen import IRGen
from asm_gen import AsmGen


def main():
    args_parser = argparse.ArgumentParser()
    args_parser.add_argument('file_in', type=str, help='name of input file')
    args_parser.add_argument('-o', type=str, default='out.asm', help='name of output file')

    compiler_args = args_parser.parse_args()
    file_in = compiler_args.file_in
    file_out = compiler_args.o

    try:
        # Tokenize stage
        create_tokens(file_in)
        # Parser stage
        root = parse(0)

        new_symbol_table = NewSymbolTable()
        ir_gen = IRGen()
        ctx = Context()

        root.make_ir(new_symbol_table, ir_gen, ctx)

        for func_name, cmds in ir_gen.cmds.items():
            print(func_name)
            for c in cmds:
                print(f'\t{c}')

        asm_gen = AsmGen(new_symbol_table, ir_gen)
        asm_gen.make_asm()
        for asm_cmd in asm_gen.cmds:
            print(asm_cmd)
    except Exception as e:
        RED_COLOR = '\033[91m'
        RESET = '\033[0m'
        print('ERROR: ', RED_COLOR + e.args[0] + RESET)
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
