from collections import OrderedDict
from utils import Regs
from symbol_table import SymbolTable
from spotmap import MemSpot, r0, r1, r2, r3, r4, bp, sp

class AsmGen:
    """State of asm generation phase."""

    def __init__(self, symbol_table: SymbolTable, ir_gen):
        self.cmds = []
        self.spotmap = {}
        self._symbol_table = symbol_table
        self._ir_gen = ir_gen
        self._regs = OrderedDict()
        for reg in (r0, r1, r2, r3, r4):
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

    def add(self, cmd):
        self.cmds.append(f'\t{cmd}')

    def make_asm(self):
        for vars in self._ir_gen.vars.values():
            offset = 0
            for v in vars:
                self.spotmap[v] = MemSpot(bp, offset)
                offset += v.c_type.size

        for func_name, cmds in self._ir_gen.cmds.items():
            self.cmds.append(f'{func_name}:')
            for c in cmds:
                if type(c) == str:
                    continue
                c.make_asm(self)

