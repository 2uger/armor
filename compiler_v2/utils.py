from collections import namedtuple
import enum

class CType(enum.Enum):
    int = 'int'

class ScopeType(enum.Enum):
    """
    Would help to differentiate local and global variables memory binding.
    Because global vars will have absolute memory address, while local would
    have offset from BP register.
    """
    LOCAL = 'local'
    GLOBAL = 'global'

CTypeSizes = {CType.int: 4}

# Represents register:
# 1, r1
# 2, r2
Reg = namedtuple('Reg', ['number', 'reg'])

class Regs:
    def __init__(self):
        self.free_regs = [Reg(n, f'r{n}') for n in range(13)]
        self.bp = Reg(13, 'bp')
        self.sp = Reg(14, 'sp')
        self.pc = Reg(15, 'pc')

    def alloc(self):
        return self.free_regs.pop(0)

    def dealloc(self, reg):
        self.free_regs.insert(0, reg)
        self.free_regs.sort(key=lambda x: x.number)

    def dealloc_many(self, regs):
        for r in regs:
            self.dealloc(r)

class StaticStorage:
    def __init__(self):
        self.start = 4096

    def place(self, value, size):
        old = self.start
        self.start += size
        return old

static_storage = StaticStorage() 
regs = Regs()