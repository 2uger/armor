from collections import OrderedDict
from utils import Regs
from symbol_table import CTypeInt, SymbolTable
from spotmap import MemSpot, r0, r1, r2, r3, r4, r5, r6, r7, r8, bp, sp
from ir import Lable
from n_asm import Push, Sub, Mov

class AsmGen:
    """State of asm generation phase."""

    def __init__(self, symbol_table: SymbolTable, ir_gen):
        self.cmds = []
        self.spotmap = {}
        self._symbol_table = symbol_table
        self._ir_gen = ir_gen
        self._regs = OrderedDict()
        for reg in (r0, r1, r2, r3, r4, r5, r6, r7):
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
        for func_name, args in self._ir_gen.func_args.items():
            offset = (-1) * (sum([arg.c_type.size for arg in args]) + 4)
            for arg in args:
                self.spotmap[arg] = MemSpot(bp, offset)
                offset += arg.c_type.size
            
        # frame pointer offset for every function
        fp_offset = {}

        for func_name, locals in self._ir_gen.func_locals.items():
            print(locals)
            if not locals:
                fp_offset[func_name] = 0
                continue
            offset = (-1) * (sum([local.c_type.size for local in locals]) + 4)
            for local in locals:
                self.spotmap[local] = MemSpot(bp, offset)
                offset += local.c_type.size
            fp_offset[func_name] = offset

        for func_name, cmds in self._ir_gen.cmds.items():
            self.clean_regs()
            self.cmds.append(f'{cmds[0].lable}:')
            # hacks, but this is prolog to function
            self.add(Push([bp]))
            self.add(Mov(bp, sp))

            frame_pointer_size = fp_offset[func_name]
            print(fp_offset)
            if frame_pointer_size:
                self.add(Sub(sp, sp, imm=frame_pointer_size))
            
            for c in cmds[1:]:
                if type(c) == str:
                    continue
                c.make_asm(self)