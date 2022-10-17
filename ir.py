from asm import Add, Mul, Sub
from utils import regs

class IRGen:
    """Intermediate representation."""

    def __init__(self):
        self.curr_func = None
        self.cmds = {}
        self.vars = {}
        self.spotmap = {}

    def add(self, ir_cmd):
        self.cmds[self.curr_func].append(ir_cmd)
    
    def new_func(self, func_name):
        self.curr_func = func_name
        self.cmds[func_name] = []
        self.vars[func_name] = {}

    def register_variable(self, var):
        """
        Register variable for current function.
        Remember where it is located.
        """
        self.vars[self.curr_func][var] = 2
        prev_off = len(self.spotmap)
        self.add(f'OFF {prev_off} {var}')
        self.spotmap[var] = prev_off + 2


global_id = 1

class IRValue:

    def __init__(self, val=None):
        global global_id
        self._id = global_id
        global_id += 1
        self.val = val
        self.spotmap = None

    def __repr__(self):
        return f'(Val:{self._id}({self.val})({self.spotmap}))'
    
class Set:
    """Move one value to another."""

    def __init__(self, dst, arg):
        self.dst = dst
        self.arg = arg
    
    def __repr__(self):
        return f'Set: {self.dst}, {self.arg}'

    def make_asm(self, spotmap, asm_gen):
        pass

class Load:

    def __init__(self, out, arg):
        self.out = out
        self.arg = arg

    def __repr__(self):
        return f'Load: {self.out}, {self.arg}'

    def make_asm(self, spotmap, asm_gen):
        pass

class FuncCall:

    def __init__(self, func, args):
        self.func = func
        self.args = args

    def __repr__(self):
        return f'Call: {self.func}, args{self.args}'

class Return:

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f'Return {self.value}'

class IRArithBinOp:
    """Binary arithmetic operations(add, sub, mul)"""

    asm_cmd = None

    def __init__(self, out, left, right):
        self.out = out,
        self.left = left,
        self.right = right
    
    def __repr__(self):
        return f'{self.asm_cmd.__name__}: {self.out}, {self.left}, {self.right}'

    def make_asm(self, asm_gen):
        l_reg = self.move_to_reg(self.left, asm_gen)
        r_reg = self.move_to_reg(self.right, asm_gen)
        res_reg = self.free_reg(asm_gen)

        asm_gen.add(self.asm_cmd(res_reg, l_reg, r_reg))

class IRAdd(IRArithBinOp):
    asm_cmd = Add

class IRSub(IRArithBinOp):
    asm_cmd = Sub

class IRMul(IRArithBinOp):
    asm_cmd = Mul