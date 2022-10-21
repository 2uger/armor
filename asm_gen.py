from collections import OrderedDict

from ir import Lable
from ir_gen import IRGen
from n_asm import Mov, Push, Sub
from spotmap import (MemSpot, bp, r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10,
                     r11, sp)
from symbol_table import NewSymbolTable


class AsmGen:
    """State of asm generation phase."""

    def __init__(self, symbol_table: NewSymbolTable, ir_gen: IRGen):
        self.cmds = []
        self.spotmap = {}
        self._symbol_table = symbol_table
        self._ir_gen = ir_gen
        self._regs = OrderedDict()
        for reg in (r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11):
            self._regs[reg] = True

    def get_reg(self):
        for reg, is_free in self._regs.items():
            if is_free:
                self._regs[reg] = False
                return reg
        raise Exception('No more free regs in usage')

    def free_regs(self, regs):
        for reg in regs:
            self._regs[reg] = True

    def clean_regs(self):
        for reg in self._regs:
            self._regs[reg] = True

    def book_reg(self, reg):
        if self._regs[reg]:
            self._regs[reg] = False
        else:
            raise Exception('try to book register, that already in use')
    
    @property
    def regs_in_use(self):
        return [r for r in self._regs if self._regs[r] == False]

    def add(self, cmd):
        self.cmds.append(f'\t{cmd}')

    def make_asm(self):
        # put global variables in file as
        # var: .word {{value}}
        if self._ir_gen.globals:
            base = 0 
            self.cmds.append('.data')
            for global_var, value in self._ir_gen.globals.items():
                self.spotmap[global_var] = MemSpot(base)
                base += global_var.c_type.size
                self.cmds.append(f'{global_var.literal}: .word {value}')
            self.cmds.append('')

        # entry point
        self.cmds.append('.text')
        self.cmds.append('.global _start')
        self.cmds.append('_start:')
        self.cmds.append('\tb main')

        # check for main function without any arguments
        if 'main' not in self._ir_gen.func_args:
            raise Exception('provide main function as entry point')
        elif len(self._ir_gen.func_args['main']) != 0:
            raise Exception('main function should not receive any arguments')

        for func_name, args in self._ir_gen.func_args.items():
            offset = 4
            for arg in args:
                self.spotmap[arg] = MemSpot(bp, offset)
                offset += arg.c_type.size
            
        # frame pointer size for every function
        fp_size = {}

        for func_name, locals in self._ir_gen.func_locals.items():
            if not locals:
                fp_size[func_name] = 0
                continue
            offset = -4
            for local in locals:
                self.spotmap[local] = MemSpot(bp, offset)
                offset -= local.c_type.size
            fp_size[func_name] = offset

        for func_name, cmds in self._ir_gen.cmds.items():
            self.clean_regs()
            self.cmds.append(f'{func_name}:')
            # hacks, but this is prolog to function
            self.add(Push([bp]))
            self.add(Mov(bp, sp))

            frame_pointer_size = fp_size[func_name]
            if frame_pointer_size:
                self.add(Sub(sp, sp, imm=abs(frame_pointer_size)))
            
            for c in cmds:
                if type(c) == Lable:
                    self.cmds.append(f'{c.lable}:')
                    continue
                if type(c) == str:
                    continue
                c.make_asm(self)
        
        self.cmds.append('')

        # put addresses of global variables at the end of the file as
        # adr_var: .word {{variable_name}}
        for global_var, value in self._ir_gen.globals.items():
            self.spotmap[global_var] = MemSpot(base)
            base += global_var.c_type.size
            self.cmds.append(f'adr_{global_var.literal}: .word {global_var.literal}')
