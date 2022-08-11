from utils import Register
import typing as t


class ASMCommand:
    cmd = None

    def __init__(self, op_dest: Register, op1: Register, op2: t.Optional[Register]=None, imm=0):
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

class Ldr(ASMCommand):
    cmd = 'ldr'

    def __repr__(self):
        if self.op2 or self.imm:
            return f'{self.cmd} {self.op_dest.reg}, [{self.op1.reg}' + (f', {self.op2.reg}]' if self.op2 else f', #{self.imm}]')
        else:
            return f'{self.cmd} {self.op_dest.reg}, [{self.op1.reg}]'
    
class Str(Ldr):
    cmd = 'str'

class Mov(ASMCommand):
    cmd = 'mov'

    def __repr__(self):
        if self.op1:
            return f'{self.cmd} {self.op_dest.reg}, {self.op1.reg}'
        else:
            return f'ldr {self.op_dest.reg}, ={self.imm}'

class Cmp(ASMCommand):
    cmd = 'cmp'

    def __repr__(self):
        return f'{self.cmd} {self.op1.reg}, {self.op2.reg}'

class Push:
    cmd = 'push'
    def __init__(self, regs: t.List[Register]):
        self.regs = regs

    def __repr__(self):
        rs = ' ,'.join([r.reg for r in self.regs])
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

    def __init__(self, location, cmp_cmd=''):
        self.location = location
        self.cmp_cmd = cmp_cmd

    def __repr__(self):
        return f'{self.cmd}{self.cmp_cmd} {self.location}'

class BL(B):
    cmd = 'bl'

class BX:
    def __init__(self, location_reg: Register, cmp_cmd=''):
        self.location_reg = location_reg
        self.cmp_cmd = cmp_cmd

    def __repr__(self):
        return f'bx{self.cmp_cmd} {self.location_reg.reg}'
    
class Data:
    """Represent data fragment in asm output."""
    def __init__(self, mem_binding, name, value):
        self.mem_binding = mem_binding
        self.name = name
        self.value = value
