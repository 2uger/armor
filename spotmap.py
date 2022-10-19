class MemSpot:
    """
    Represents memory spot
    Absolute for variables in .data section
    Relative from base pointer for local function variables
    """
    def __init__(self, base, offset):
        self.base = base
        self.offset = offset

    @property
    def asm_str(self):
        if type(self.base) == RegSpot:
            return f'{bp.asm_str}, #{self.offset}'
        else:
            return f'{self.base}'

class RegSpot:
    """Register spot"""
    def __init__(self, name):
        self.name = name

    @property
    def asm_str(self):
        return self.name

    @property
    def reg(self):
        return self.name

r0 = RegSpot('r0')
r1 = RegSpot('r1')
r2 = RegSpot('r2')
r3 = RegSpot('r3')
r4 = RegSpot('r4')
r5 = RegSpot('r5')
r6 = RegSpot('r6')
r7 = RegSpot('r7')
r8 = RegSpot('r8')
r9 = RegSpot('r9')
bp = RegSpot('bp')
sp = RegSpot('sp')
lr = RegSpot('lr')
