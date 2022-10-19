from n_asm import Str, Ldr, Add, Mul, Sub, Mov
import n_asm
from spotmap import MemSpot, RegSpot, bp, sp, r0, lr

class IRGen:
    """Intermediate representation."""

    def __init__(self):
        self.curr_func = None
        self.cmds = {}
        # function arguments
        self.func_args = {}
        # function locals
        self.func_locals = {}
        self.globals = []

    def add(self, ir_cmd):
        self.cmds[self.curr_func].append(ir_cmd)
    
    def new_func(self, func_name):
        self.curr_func = func_name
        self.cmds[func_name] = []

        self.func_args[func_name] = []
        self.func_locals[func_name] = []

    def register_argument(self, val):
        """Register function argument."""
        self.func_args[self.curr_func].append(val)

    def register_local(self, val):
        """Register function local variable."""
        self.func_locals[self.curr_func].append(val)

    def register_global(self, val):
        """Register global variable."""
        self.globals.append(val)


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

    def _move_to_reg(self, asm_gen, value):
        spot = asm_gen.spotmap.get(value)
        if spot:
            dst_reg = None
            if type(spot) == RegSpot:
                dst_reg = spot
            else:
                dst_reg = asm_gen.get_reg()
                asm_gen.add(Ldr(dst_reg, spot))
            return dst_reg
        if type(value.literal) == int:
            dst_reg = asm_gen.get_reg()
            asm_gen.add(Mov(dst_reg, None, imm=value.literal))
            return dst_reg
        raise Exception(f'_move_to_reg: do not know what to move: {value}') 


class Set(IRCmd):
    """Move one value to another."""

    def __init__(self, dst, arg):
        self.dst = dst
        self.arg = arg
    
    def __repr__(self):
        return f'Set: {self.dst}, {self.arg}'

    def make_asm(self, asm_gen):
        arg_reg = self._move_to_reg(asm_gen, self.arg)
        spot = asm_gen.spotmap[self.dst]

        asm_gen.add(Str(arg_reg, spot))

        asm_gen.free_regs([arg_reg])

class FuncCall(IRCmd):

    def __init__(self, out, func, args):
        self._out = out
        self._func = func
        self._args = args

    def __repr__(self):
        return f'Call: {self._func}, args{self._args}, out: {self._out}'

    def make_asm(self, asm_gen):
        # save local registers
        regs_in_use = asm_gen.regs_in_use
        if regs_in_use:
            asm_gen.add(n_asm.Push(regs_in_use))

        # push arguments
        arguments = [self._move_to_reg(asm_gen, arg) for arg in self._args]
        if arguments:
            asm_gen.add(n_asm.Push(arguments))

        asm_gen.add(n_asm.BL(self._func))
        # Location of function result
        asm_gen.spotmap[self._out] = r0

        # clean stack from function arguments
        if arguments:
            asm_gen.add(n_asm.Sub(sp, sp, None, imm=len(arguments) * 4))

        # restore saved local registers
        if regs_in_use:
            asm_gen.add(n_asm.Pop(regs_in_use))

class Return(IRCmd):

    def __init__(self, value):
        self._value = value

    def __repr__(self):
        return f'Return {self._value}'

    def make_asm(self, asm_gen):
        res_reg = self._move_to_reg(asm_gen, self._value)
        if res_reg != r0:
            # store return value
            asm_gen.add(n_asm.Mov(r0, res_reg))
        asm_gen.free_regs([res_reg])

        # function epilogue
        asm_gen.add(n_asm.Mov(sp, bp))
        asm_gen.add(n_asm.Pop([bp]))
        asm_gen.add(n_asm.BX(lr))

class Cmp(IRCmd):

    def __init__(self, left, right):
        self._left = left
        self._right = right

    def __repr__(self):
        return f'Cmp: {self._left} to {self._right}'

    def make_asm(self, asm_gen):
        l_reg = self._move_to_reg(asm_gen, self._left)
        r_reg = self._move_to_reg(asm_gen, self._right)

        asm_gen.add(n_asm.Cmp(l_reg, r_reg))

class Jmp(IRCmd):

    def __init__(self, lable, cond=''):
        self._lable = lable
        self._cond = cond

    def __repr__(self):
        return f'Jmp: {self._lable}'
    
    def make_asm(self, asm_gen):
        asm_gen.add(n_asm.B(self._lable, self._cond))

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
    
class Add(ArithBinOp):
    asm_cmd = Add

class Sub(ArithBinOp):
    asm_cmd = Sub

class Mul(ArithBinOp):
    asm_cmd = Mul

lable_num = 0

class Lable:

    def __init__(self, name=None):
        global lable_num
        self.lable = 'lable{}_{}'.format(f'_{name}' if name else '', lable_num)
        lable_num += 1

    def __repr__(self):
        return self.lable
