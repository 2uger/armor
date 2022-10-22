import ir
from ir_gen import IRGen
from symbol_table import CTypeInt, NewSymbolTable, ScopeType


class EmptyNode:
    """Empty node."""
    def make_ir(self, *args, **kwargs):
        pass

class Program:
    """Main node of the whole program."""
    def __init__(self, nodes):
        self.nodes = nodes

    def make_ir(self, symbol_table, ir_gen, ctx):
        for n in self.nodes:
            n.make_ir(symbol_table, ir_gen, ctx)

class Declaration:
    def __init__(self, node, body=None):
        self.node = node
        # only for function declaration
        self.body = body
    
    def __repr__(self):
        return f'{self.node}: {self.body}'

    def make_ir(self, symbol_table: NewSymbolTable, ir_gen: IRGen, ctx):
        spec = self.node.spec
        decl = self.node.decl
        init = self.node.init
        if isinstance(decl, Identifier):
            # Do not allow multiple declaration for variable even in different scopes
            if symbol_table.lookup(decl.identifier):
                raise Exception(f'multiple declaration of {decl.identifier}')

            if ctx.is_global:
                val = symbol_table.add_variable(decl.identifier, CTypeInt, ScopeType.GLOBAL)
                if type(init) != Number:
                    raise Exception('only constant values for global variables')
                out = init.make_ir(symbol_table, ir_gen, ctx)

                if type(out.literal) != int:
                    raise Exception(f'non integer literal for global variable: {decl.identifier}')
                ir_gen.register_global(val, init.number)
            else:
                val = symbol_table.add_variable(decl.identifier)
                ir_gen.register_local(val)
                if init:
                    out = init.make_ir(symbol_table, ir_gen, ctx)
                    ir_gen.add(ir.Set(val, out))
        elif isinstance(decl, Function):
            ctx.set_global(False)
            ir_gen.new_func(decl.identifier.identifier)

            symbol_table.add_variable(decl.identifier.identifier, CTypeInt, ScopeType.GLOBAL)

            for parm in decl.parms:
                var = symbol_table.add_variable(parm.decl.identifier)
                ir_gen.register_argument(var)

            self.body.make_ir(symbol_table, ir_gen, ctx)

            ctx.set_global(True)

        else:
            raise Exception(f'bullshit declaration: {self}')

class DeclarationRoot:
    def __init__(self, spec, decl, init=None):
        """
        Params:
        spec - specifier like int, char
        decl - declaration of the symbol, it might be Identifier or Function
        init - initial value
        """
        self.spec = spec
        self.decl = decl
        self.init = init

    def __repr__(self):
        return f'{self.spec} {self.decl} = {self.init}'

class Function:
    def __init__(self, identifier, parms):
        self.identifier = identifier
        self.parms = parms

    def __repr__(self):
        return f'{self.identifier}: {self.parms}'

class FuncCall:
    """Represents function call."""
    def __init__(self, func_name, args):
        self._func_name = func_name
        self._args = args

    def make_ir(self, symbol_table, ir_gen, ctx):
        if ctx.is_global:
            raise Exception(f'can\'t call function {self._func_name} in global context')
        
        if not symbol_table.lookup(self._func_name):
            raise Exception(f'call to unknown function')
        
        self._check_args(ir_gen)

        args = [arg.make_ir(symbol_table, ir_gen, ctx) for arg in self._args]
        out = ir.IRValue(CTypeInt)
        ir_gen.add(ir.FuncCall(out, self._func_name, args))

        return out
    
    def _check_args(self, ir_gen):
        func_parms = ir_gen.func_args.get(self._func_name)
        if len(func_parms) != len(self._args):
            raise Exception(f'wrong amount of arguments to function call: {self._func_name}')

class Compound:
    """List of expressions."""
    def __init__(self, items):
        self.items = items

    def make_ir(self, symbol_table, ir_gen, ctx):
        symbol_table.open_scope()
        for item in self.items:
            item.make_ir(symbol_table, ir_gen, ctx)
        symbol_table.close_scope()

    def __repr__(self):
        r = '{\n'
        for i in self.items:
            r += str(i) + '\n'
        r += '}'
        return r

class Return:
    def __init__(self, ret_expr):
        self.ret_expr = ret_expr

    def __repr__(self):
        return f'return {self.ret_expr}'

    def make_ir(self, symbol_table, ir_gen, ctx):
        out = self.ret_expr.make_ir(symbol_table, ir_gen, ctx)
        ir_gen.add(ir.Return(out))

class IfStatement:
    def __init__(self, cond, stmt, else_stmt):
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt

    def make_ir(self, symbol_table, ir_gen, ctx):
        # do no generate any code if both of statements is empty
        if not((self.stmt and self.stmt.items) or (self.else_stmt and self.else_stmt.items)):
            return

        if not isinstance(self.cond, Relational):
            # everything is true inside if statement if result of expression != 0
            out = self.cond.make_ir(symbol_table, ir_gen, ctx)
            ir_gen.add(ir.Cmp(out, ir.IRValue(CTypeInt, 0)))
            cond_cmd = 'eq'
        else:
            self.cond.make_ir(symbol_table, ir_gen, ctx)
            cond_cmd = self.cond.n_cond

        lable_end = ir.Lable('end')
        if self.else_stmt:
            lable_else = ir.Lable('else')

            ir_gen.add(ir.Jmp(lable_else, cond_cmd))

            self.stmt.make_ir(symbol_table, ir_gen, ctx)

            ir_gen.add(ir.Jmp(lable_end))
            ir_gen.add(lable_else)

            self.else_stmt.make_ir(symbol_table, ir_gen, ctx)

            ir_gen.add(lable_end)
        else:
            ir_gen.add(ir.Jmp(lable_end, cond_cmd))
            self.stmt.make_ir(symbol_table, ir_gen, ctx)

            ir_gen.add(lable_end)

class Equals:
    """Expression for assignment."""
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

    def make_ir(self, symbol_table, ir_gen, ctx):
        if not isinstance(self.left, Identifier):
            raise Exception('only identifier could be on left side of equal sign')
        
        dst = self.left.make_ir(symbol_table, ir_gen, ctx)
        res = self.right.make_ir(symbol_table, ir_gen, ctx)

        ir_gen.add(ir.Set(dst, res))

class ArithBinOp:
    ir_cmd = None

    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def make_ir(self, symbol_table, ir_gen, ctx):
        """Create ir code for binary operation."""
        if type(self.left) == Number and type(self.right) == Number:
            return ir.IRValue(CTypeInt, self.left.number + self.right.number)

        out = ir.IRValue(CTypeInt)
        
        l_val = self.left.make_ir(symbol_table, ir_gen, ctx)
        r_val = self.right.make_ir(symbol_table, ir_gen, ctx)
        ir_cmd = self.ir_cmd(out, l_val, r_val)
        ir_gen.add(ir_cmd)

        return out

    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

class Plus(ArithBinOp):
    ir_cmd = ir.Add

class Minus(ArithBinOp):
    ir_cmd = ir.Sub

class Mul(ArithBinOp):
    ir_cmd = ir.Mul

class Relational(ArithBinOp):
    # cond shows when relational expression is NOT true => when to jump to different location
    cond = None

    def __init__(self, left, right, op):
        super().__init__(left, right, op)

    def make_ir(self, symbol_table, ir_gen, ctx):
        l_val = self.left.make_ir(symbol_table, ir_gen, ctx)
        r_val = self.right.make_ir(symbol_table, ir_gen, ctx)
        ir_gen.add(ir.Cmp(l_val, r_val))

class LessThan(Relational):
    cond = 'lt'
    n_cond = 'ge'

class LessEqual(Relational):
    cond = 'le'
    n_cond = 'gt'

class BiggerThan(Relational):
    cond = 'gt'
    n_cond = 'le'

class BiggerEqual(Relational):
    cond = 'ge'
    n_cond = 'lt'

class NotEqual(Relational):
    cond = 'ne'
    n_cond = 'eq'

class Equal(Relational):
    cond = 'eq'
    n_cond = 'ne'

class Identifier:
    def __init__(self, identifier):
        self.identifier = identifier

    def __repr__(self):
        return f'{self.identifier}'

    def make_ir(self, symbol_table, ir_gen, ctx):
        var = symbol_table.lookup(self.identifier)
        if not var:
            raise Exception(f'unregistered identifier: {self.identifier}')
        return var

class Number:
    def __init__(self, number):
        self.number = number

    def __repr__(self):
        return f'Number: {self.number}'

    def make_ir(self, *args):
        return ir.IRValue(CTypeInt, self.number)
