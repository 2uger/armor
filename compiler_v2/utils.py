from collections import namedtuple
import enum

class ScopeType(enum.Enum):
    """
    Would help to differentiate local and global variables memory binding.
    Because global vars will have absolute memory address, while local would
    have offset from BP register.
    """
    LOCAL = 'local'
    GLOBAL = 'global'

# Describe C types
class CTypeSpec(enum.Enum):
    int = 'int'
    char = 'char'

CType = namedtuple('CType', ['size'])
CTypeInt = CType(4)
CTypeChar = CType(2)

# Represents register:
# 1, r1
# 2, r2
Register = namedtuple('Register', ['number', 'reg'])

class Regs:
    def __init__(self):
        self.free_regs = [Register(n, f'r{n}') for n in range(13)]
        self.bp = Register(13, 'bp')
        self.sp = Register(14, 'sp')
        self.pc = Register(15, 'pc')

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