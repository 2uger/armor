from collections import OrderedDict

from spotmap import (
    MemSpot,
    r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, bp, sp
)
from ir import Lable
from n_asm import Push, Sub, Mov
from symbol_table import NewSymbolTable

class AsmGen:
    """State of asm generation phase."""

    def __init__(self, symbol_table: NewSymbolTable, ir_gen):
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
    
    @property
    def regs_in_use(self):
        return [r for r in self._regs if self._regs[r] == False]

    def add(self, cmd):
        self.cmds.append(f'\t{cmd}')

    def make_asm(self):
        base = 4096 
        self.cmds.append('.data')
        for global_var, value in self._ir_gen.globals.items():
            print('Global ', global_var, base)
            self.spotmap[global_var] = MemSpot(base)
            base += global_var.c_type.size
            self.cmds.append(f'{global_var.literal}: .word {value}')

        # entry point
        self.cmds.append('')
        self.cmds.append('.text')
        self.cmds.append('.global _start')
        self.cmds.append('_start:')
        self.cmds.append('\tb main')

        for func_name, args in self._ir_gen.func_args.items():
            offset = 4
            for arg in args:
                print('Local ', arg, offset)
                self.spotmap[arg] = MemSpot(bp, offset)
                offset += arg.c_type.size
            
        # frame pointer size for every function
        fp_size = {}

        for func_name, locals in self._ir_gen.func_locals.items():
            if not locals:
                fp_size[func_name] = 0
                continue
            offset = 0
            for local in locals:
                self.spotmap[local] = MemSpot(bp, offset)
                offset += local.c_type.size
            fp_size[func_name] = offset

        for func_name, cmds in self._ir_gen.cmds.items():
            self.clean_regs()
            self.cmds.append(f'{func_name}:')
            # hacks, but this is prolog to function
            self.add(Push([bp]))
            self.add(Mov(bp, sp))

            frame_pointer_size = fp_size[func_name]
            print(fp_size)
            if frame_pointer_size:
                self.add(Sub(sp, sp, imm=frame_pointer_size))
            
            for c in cmds:
                if type(c) == Lable:
                    self.cmds.append(f'{c.lable}:')
                    continue
                if type(c) == str:
                    continue
                c.make_asm(self)
        
        self.cmds.append('')

        for global_var, value in self._ir_gen.globals.items():
            print('Global ', global_var, base)
            self.spotmap[global_var] = MemSpot(base)
            base += global_var.c_type.size
            self.cmds.append(f'adr_{global_var.literal}: .word {global_var.literal}')
