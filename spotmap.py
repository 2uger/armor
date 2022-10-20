class MemSpot:
    """
    Represents memory spot
    Absolute for variables in .data section
    Relative from base pointer for local function variables
    """
    def __init__(self, base, offset=None):
        self._base = base
        self._offset = offset

    @property
    def asm_str(self):
        if type(self._base) == RegSpot:
            return f'{bp.name}, #{self._offset}'
        else:
            return f'{self._base}'

class RegSpot:
    """Register spot"""
    def __init__(self, name):
        self._name = name

    def __repr__(self):
        return f'Reg: {self._name}'
    @property
    def asm_str(self):
        return self._name

    @property
    def reg(self):
        return self._name

    @property
    def name(self):
        return self._name

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
r10 = RegSpot('r10')
r11 = RegSpot('r11')
bp = RegSpot('bp')
sp = RegSpot('sp')
lr = RegSpot('lr')
