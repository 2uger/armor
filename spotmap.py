class MemSpot:
    """
    Represents memory spot
    Absolute for variables in .data section
    Relative from base pointer for local function variables
    """
    def __init__(self, base, offset):
        self.base = base
        self.offset = offset

    def asm_str(self):
        if type(self.base) == RegSpot:
            return f'bp, {self.offset}'
        else:
            return f'{self.base}'

class RegSpot:
    """Register spot"""
    def __init__(self, name):
        self.name = name

    @property
    def reg(self):
        return self.name

r0 = RegSpot('r0')
r1 = RegSpot('r1')
r2 = RegSpot('r2')
r3 = RegSpot('r3')
r4 = RegSpot('r4')
r5 = RegSpot('r5')
bp = RegSpot('bp')
sp = RegSpot('sp')
