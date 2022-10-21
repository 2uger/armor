import n_asm
from n_asm import Add, Ldr, Mov, Mul, Str, Sub
from spotmap import MemSpot, RegSpot, bp, lr, r0, sp

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
                if type(spot._base) != RegSpot:
                    tmp_reg = asm_gen.get_reg()
                    dst_reg = asm_gen.get_reg()

                    asm_gen.add(Ldr(tmp_reg, value.literal))
                    asm_gen.add(Ldr(dst_reg, tmp_reg))

                    asm_gen.free_regs([tmp_reg])
                else:
                    dst_reg = asm_gen.get_reg()
                    asm_gen.add(Ldr(dst_reg, spot))
            return dst_reg
        elif type(value.literal) == int:
            dst_reg = asm_gen.get_reg()
            asm_gen.add(Mov(dst_reg, None, imm=value.literal))
            return dst_reg
        else:
            raise Exception(f'_move_to_reg: don\'t got spot for: {value}') 

class Set(IRCmd):
    """Move one value to another."""

    def __init__(self, out, arg):
        self._out = out
        self._arg = arg
    
    def __repr__(self):
        return f'Set: {self._out}, {self._arg}'

    def make_asm(self, asm_gen):
        regs_to_free = []
        arg_reg = self._move_to_reg(asm_gen, self._arg)
        spot = asm_gen.spotmap[self._out]

        if type(spot) == MemSpot and type(spot._base) != RegSpot:
            tmp_reg = asm_gen.get_reg()

            asm_gen.add(Ldr(tmp_reg, self.dst.literal))
            asm_gen.add(Str(arg_reg, tmp_reg))

            regs_to_free = [tmp_reg, arg_reg]
        else:
            asm_gen.add(Str(arg_reg, spot))
            regs_to_free = [arg_reg]

        asm_gen.free_regs(regs_to_free)

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
        arguments_regs = [self._move_to_reg(asm_gen, arg) for arg in self._args]
        if arguments_regs:
            asm_gen.add(n_asm.Push(arguments_regs))
        asm_gen.free_regs(arguments_regs)

        asm_gen.add(n_asm.BL(self._func))
        # Location of function output
        asm_gen.spotmap[self._out] = r0
        asm_gen.book_reg(r0)

        # clean stack from function arguments
        if arguments_regs:
            asm_gen.add(n_asm.Add(sp, sp, None, imm=len(arguments_regs) * 4))

        # restore saved local registers
        if regs_in_use:
            asm_gen.add(n_asm.Pop(regs_in_use))

class Return(IRCmd):

    def __init__(self, arg):
        self._arg = arg

    def __repr__(self):
        return f'Return {self._arg}'

    def make_asm(self, asm_gen):
        res_reg = self._move_to_reg(asm_gen, self._arg)
        if res_reg != r0:
            # store return value
            asm_gen.add(n_asm.Mov(r0, res_reg))
        asm_gen.free_regs([res_reg])

        # # function epilogue
        # fp_size = asm_gen._ir_gen.func_locals[]
        asm_gen.add(n_asm.Mov(sp, bp))
        asm_gen.add(n_asm.Pop([bp]))
        asm_gen.add(n_asm.BX(lr))

class Cmp(IRCmd):

    def __init__(self, arg_1, arg_2):
        self._arg_1 = arg_1
        self._arg_2 = arg_2

    def __repr__(self):
        return f'Cmp: {self._arg_1} to {self._arg_2}'

    def make_asm(self, asm_gen):
        l_reg = self._move_to_reg(asm_gen, self._arg_1)
        r_reg = self._move_to_reg(asm_gen, self._arg_2)

        asm_gen.add(n_asm.Cmp(l_reg, r_reg))

        asm_gen.free_regs([l_reg, r_reg])

class Jmp(IRCmd):

    def __init__(self, lable, cond=''):
        self._lable = lable
        self._cond = cond

    def __repr__(self):
        return f'Jmp_{self._cond}: {self._lable}'
    
    def make_asm(self, asm_gen):
        asm_gen.add(n_asm.B(self._lable, self._cond))

class ArithBinOp(IRCmd):
    """Binary arithmetic operations(add, sub, mul)"""

    asm_cmd = None

    def __init__(self, out, arg_1, arg_2):
        self._out = out
        self._arg_1 = arg_1
        self._arg_2 = arg_2
    
    def __repr__(self):
        return f'{self.asm_cmd.__name__}: {self._out}, {self._arg_1}, {self._arg_2}'

    def make_asm(self, asm_gen):
        l_reg = self._move_to_reg(asm_gen, self._arg_1)
        r_reg = self._move_to_reg(asm_gen, self._arg_2)
        res_reg = asm_gen.get_reg()

        asm_gen.add(self.asm_cmd(res_reg, l_reg, r_reg))

        asm_gen.free_regs([l_reg, r_reg])

        asm_gen.spotmap[self._out] = res_reg
    
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
