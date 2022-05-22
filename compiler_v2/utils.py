import bisect
import enum

class CType(enum.Enum):
    int = 'int'

class Regs:
    def __init__(self):
        self.free_regs = [r for r in range(15)]

    def alloc(self):
        return self.free_regs.pop(0)

    def dealloc(self, reg):
        bisect.insort(self.free_regs, reg)

    def dealloc_many(self, regs):
        for r in regs:
            bisect.insort(self.free_regs, r)

class StaticStorage:
    def __init__(self):
        self.start = 4096

    def place(self, value, size):
        old = self.start
        self.start += size
        return old

static_storage = StaticStorage() 
regs = Regs()

CTypeSizes = {CType.int: 4}
