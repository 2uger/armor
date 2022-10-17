from asm import Add, Mul, Sub, Mov, Ldr, Str
from spotmap import RegSpot, bp

class IRGen:
    """Intermediate representation."""

    def __init__(self):
        self.curr_func = None
        self.cmds = {}
        self.vars = {}

    def add(self, ir_cmd):
        self.cmds[self.curr_func].append(ir_cmd)
    
    def new_func(self, func_name):
        self.curr_func = func_name
        self.cmds[func_name] = []
        self.vars[func_name] = []

    def register_variable(self, var):
        """
        Register variable for current function.
        Remember where it is located.
        """
        self.vars[self.curr_func].append(var)


global_id = 1

class IRValue:

    def __init__(self, c_type, literal=None):
        global global_id
        self._id = global_id
        global_id += 1
        # literal could be:
        # int - represents number literal
        # string - represents string literal
        self.literal = literal
        self.c_type = c_type

    def __repr__(self):
        return f'(Val:{self._id}({self.literal}))'
    
class IRCmd:
    """Base class for IR command."""

    def make_asm(self, asm_gen):
        raise NotImplementedError

class Set(IRCmd):
    """Move one value to another."""

    def __init__(self, dst, arg):
        self.dst = dst
        self.arg = arg
    
    def __repr__(self):
        return f'Set: {self.dst}, {self.arg}'

    def make_asm(self, asm_gen):
        arg_reg = asm_gen.spotmap[self.arg]

        addr_reg = asm_gen.get_reg()
        spot = asm_gen.spotmap[self.dst]
        asm_gen.add(Str(addr_reg, bp, None, spot.asm_str()))

        asm_gen.free_regs([arg_reg])

class Load(IRCmd):

    def __init__(self, out, arg):
        self.out = out
        self.arg = arg

    def __repr__(self):
        return f'Load: {self.out}, {self.arg}'

    def make_asm(self, asm_gen):
        pass

class FuncCall(IRCmd):

    def __init__(self, func, args):
        self.func = func
        self.args = args

    def __repr__(self):
        return f'Call: {self.func}, args{self.args}'

class Return(IRCmd):

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f'Return {self.value}'

class ArithBinOp(IRCmd):
    """Binary arithmetic operations(add, sub, mul)"""

    asm_cmd = None

    def __init__(self, out, left, right):
        self.out = out
        self.left = left
        self.right = right
    
    def __repr__(self):
        return f'{self.asm_cmd.__name__}: {self.out}, {self.left}, {self.right}'

    def make_asm(self, asm_gen):
        l_reg = self._move_to_reg(asm_gen, self.left)
        r_reg = self._move_to_reg(asm_gen, self.right)
        res_reg = asm_gen.get_reg()

        asm_gen.add(self.asm_cmd(res_reg, l_reg, r_reg))

        asm_gen.free_regs([l_reg, r_reg])

        asm_gen.spotmap[self.out] = res_reg
    
    def _move_to_reg(self, asm_gen, value):
        dst_reg = asm_gen.get_reg()
        if type(value.literal) == int:
            asm_gen.add(Mov(dst_reg, None, imm=value.literal))
        elif type(value.literal) == str:
            spot = asm_gen.spotmap[value]
            if type(spot) == RegSpot:
                asm_gen.add(Mov(dst_reg, spot))
            else:
                asm_gen.add(Ldr(dst_reg, bp, None, spot.asm_str()))
        return dst_reg

class Add(ArithBinOp):
    asm_cmd = Add

class Sub(ArithBinOp):
    asm_cmd = Sub

class Mul(ArithBinOp):
    asm_cmd = Mul