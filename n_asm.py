import typing as t

from spotmap import RegSpot


class ASMCommand:
    cmd = None

    def __init__(self, op_dest: RegSpot, op1: RegSpot, op2: t.Optional[RegSpot]=None, imm=0):
        self.op_dest = op_dest
        self.op1 = op1
        self.op2 = op2
        self.imm = imm
    
    def __repr__(self):
        if self.op2:
            return f'{self.cmd} {self.op_dest.reg}, {self.op1.reg}, {self.op2.reg}'
        else:
            return f'{self.cmd} {self.op_dest.reg}, {self.op1.reg}, #{self.imm}'

class Add(ASMCommand):
    cmd = 'add'

class Sub(ASMCommand):
    cmd = 'sub'

class Mul(ASMCommand):
    cmd = 'mul'

class Ldr:
    cmd = 'ldr'

    def __init__(self, dst_reg, src_addr=None, imm_val=None):
        self._dst_reg = dst_reg
        self._src_addr = src_addr
        self._imm_val = imm_val

    def __repr__(self):
        # hacks, in this case it means this
        # var: .word 2
        # adr_var: .word r - src_addr
        if type(self._src_addr) == str:
            return f'{self.cmd} {self._dst_reg.name}, adr_{self._src_addr}'

        if self._src_addr:
            return f'{self.cmd} {self._dst_reg.name}, [{self._src_addr.asm_str}]'
        else:
            return f'{self.cmd} {self._dst_reg.name}, ={self._imm_val}'
    
class Str(Ldr):
    cmd = 'str'

class Mov(ASMCommand):
    cmd = 'mov'

    def __repr__(self):
        if self.op1:
            return f'{self.cmd} {self.op_dest.reg}, {self.op1.reg}'
        else:
            return f'ldr {self.op_dest.reg}, ={self.imm}'

class Cmp:
    cmd = 'cmp'

    def __init__(self, l_reg, r_reg):
        self._l_reg = l_reg
        self._r_reg = r_reg

    def __repr__(self):
        return f'{self.cmd} {self._l_reg.name}, {self._r_reg.name}'

class Push:
    cmd = 'push'
    def __init__(self, regs):
        self._regs = regs

    def __repr__(self):
        rs = ', '.join([r.name for r in self._regs])
        return f'{self.cmd} {{{rs}}}'

class Pop(Push):
    cmd = 'pop'

class Lable:
    def __init__(self, lable):
        self.lable = lable
    
    def __repr__(self):
        return f'{self.lable}:'

class B:
    cmd = 'b'

    def __init__(self, lable, cond=''):
        self._lable = lable
        self._cond = cond

    def __repr__(self):
        return f'{self.cmd}{self._cond} {self._lable}'


class BL(B):
    cmd = 'bl'

class BX:
    def __init__(self, location_reg: RegSpot, cmp_cmd=''):
        self.location_reg = location_reg
        self.cmp_cmd = cmp_cmd

    def __repr__(self):
        return f'bx{self.cmp_cmd} {self.location_reg.reg}'