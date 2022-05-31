"""Assembly commands"""

class ASMCommand:
    cmd = None
    def __init__(self, op_dest, op1, op2=None, imm=0):
        self.op_dest = op_dest
        self.op1 = op1
        self.op2 = op2
        self.imm = imm
    
    def __repr__(self):
        if self.op2:
            return f'{self.cmd} r{self.op_dest}, r{self.op1}, r{self.op2}'
        else:
            return f'{self.cmd} r{self.op_dest}, r{self.op1}, #{self.imm}'

class Add(ASMCommand):
    cmd = 'add'

class Minus(ASMCommand):
    cmd = 'sub'

class Mul(ASMCommand):
    cmd = 'mul'

class Ldr(ASMCommand):
    cmd = 'ldr'

    def __repr__(self):
        if self.op2 or self.imm:
            return f'{self.cmd} r{self.op_dest}, [r{self.op1}' + (f', r{self.op2}]' if self.op2 else f', #{self.imm}]')
        else:
            return f'{self.cmd} r{self.op_dest}, [r{self.op1}]'
    
class Str(Ldr):
    cmd = 'str'

class Mov(ASMCommand):
    cmd = 'mov'

    def __repr__(self):
        return f'{self.cmd} r{self.op_dest}, ' + (f'r{self.op1}' if self.op1 else f'#{self.imm}')

class Cmp(ASMCommand):
    cmd = 'cmp'

    def __repr__(self):
        return f'{self.cmd} r{self.op1}, r{self.op2}'

class Push:
    cmd = 'push'
    def __init__(self, regs):
        self.regs = regs

    def __repr__(self):
        rs = ' ,'.join([f'r{r}' for r in self.regs])
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
        return f'b{self.cmp_cmd} {self.location}'

class BL(B):
    cmd = 'bl'

class BX(B):
    cmd = 'bx'

    def __repr__(self):
        return f'b{self.cmp_cmd} r{self.location}'