from utils import Regs
from symbol_table import SymbolTable

class AsmGen:
    """State of asm generation phase."""

    def __init__(self, symbol_table: SymbolTable, ir_gen):
        self.cmds = []
        self.spotmap = {}
        self._symbol_table = symbol_table
        self._ir_gen = ir_gen
        self._regs = Regs()

    def get_reg(self):
        return self._regs.alloc()

    def free_regs(self, regs):
        self._regs.dealloc_many(regs)

    def add(self, cmd):
        self.cmds.append(cmd)

    def make_asm(self):
        for f, vars in self._ir_gen.vars.items():
            print('Vars')
            print(vars)
            off = 0
            for v in vars:
                self.spotmap[v] = off
                off += 2
        for func_name, cmds in self._ir_gen.cmds.items():
            print(func_name)
            for c in cmds:
                if type(c) == str:
                    continue
                c.make_asm(self)

