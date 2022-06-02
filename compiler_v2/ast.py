from symbol_table import SymbolTable
import utils as u
import asm as asm

class EmptyNode:
    """Empty node."""
    def make_asm(self, *args, **kwargs):
        pass

class Programm:
    """Main node of the whole programm."""
    def __init__(self, nodes):
        self.nodes = nodes

    def make_asm(self, symbol_table, code):
        for n in self.nodes:
            n.make_asm(symbol_table, code)

class Declaration:
    def __init__(self, node, body=None):
        """
        body - only for functions
        """
        self.node = node
        self.body = body
    
    def __repr__(self):
        return f'{self.node}: {self.body}'

    def make_asm(self, symbol_table: SymbolTable, code):
        spec = self.node.spec
        decl = self.node.decl
        init = self.node.init

        if isinstance(decl, Identifier):
            if symbol_table.lookup(decl.identifier):
                raise Exception(f'Multiple declaration for: {decl.identifier}')

            # TODO: after add new types - change this
            c_type = u.CTypeInt

            mem_binding = u.static_storage.place(None, c_type.size)
            if init:
                res_reg = init.make_asm(symbol_table, code)
                free_reg = u.regs.alloc()
                code.extend([asm.Mov(free_reg, None, imm=mem_binding), asm.Str(res_reg, free_reg)])
                u.regs.dealloc_many([res_reg, free_reg])
            symbol_table.add_symbol(decl.identifier, c_type, mem_binding, u.ScopeType.GLOBAL)
        elif isinstance(decl, Function):
            symbol_table.open_scope()

            # STACK:
            # arg-1
            # arg-2
            # ...
            # arg-n
            # ret_value
            # ret_address
            # old_bp
            # BP - frame pointer for current function points here

            # bp offset for first argument
            bp_offset = 4 + 4 + 4 + u.CTypeInt.size * len(decl.args)

            for parm in decl.args:
                parm_name = parm.decl.identifier
                c_type = u.CTypeInt
                symbol_table.add_symbol(parm_name, c_type, bp_offset, u.ScopeType.LOCAL)
                bp_offset -= c_type.size

            code.extend(['\n', asm.Lable(decl.identifier), asm.Push([u.regs.bp]), asm.Mov(u.regs.bp, u.regs.sp)])
            self.body.make_asm(symbol_table, code)

            symbol_table.close_scope()
        else:
            raise Exception(f'Bullshit declaration: {self}')

class DeclarationRoot:
    def __init__(self, spec, decl, init=None):
        """
        Param:
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
    def __init__(self, identifier, args):
        self.identifier = identifier
        self.args = args

    def __repr__(self):
        return f'{self.identifier}: {self.args}'

class FuncCall:
    """Represents function call."""
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def make_asm(self, symbol_table, code):
        # TODO: save registers in use
        args_to_push = []

        for arg in self.args:
            args_to_push.append(arg.make_asm(symbol_table, code))

        # Push arguments
        if args_to_push:
            code.append(asm.Push(args_to_push))
            u.regs.dealloc_many(args_to_push)
        # Push space for return value
        code.append(asm.Add(u.regs.sp, u.regs.sp, imm=u.CTypeInt.size))
        # Call function
        code.append(asm.BL(self.func))

        # Collect return value
        free_reg = u.regs.alloc()
        res_reg = u.regs.alloc()
        code.extend([asm.Minus(free_reg, u.regs.sp, imm=u.CTypeInt.size), asm.Str(res_reg, free_reg)])
        u.regs.dealloc(free_reg)

        # Clean up stack from arguments
        code.append(asm.Minus(u.regs.sp, u.regs.sp, imm=4 * 2 + u.CTypeInt.size * len(args_to_push)))

        return res_reg

class ExprStmt:
    """Single Expression statement."""
    def __init__(self, expr):
        self.expr = expr

class Compound:
    def __init__(self, items):
        self.items = items

    def __repr__(self):
        r = '{\n'
        for i in self.items:
            r += str(i) + '\n'
        r += '}'
        return r

    def make_asm(self, symbol_table, code):
        for item in self.items:
            print(item)
            item.make_asm(symbol_table, code)

class Return:
    def __init__(self, ret_expr):
        self.ret_expr = ret_expr

    def __repr__(self):
        return f'return {self.ret_expr}'

    def make_asm(self, symbol_table, code):
        res_reg = self.ret_expr.make_asm(symbol_table, code)
        free_reg = u.regs.alloc()
        # Store result in return value place
        code.extend([asm.Minus(free_reg, u.regs.bp, imm=u.CTypeInt.size * 2), asm.Str(res_reg, free_reg)])
        # Return from function
        code.extend([asm.Pop([u.regs.bp, free_reg]), asm.BX(free_reg)])
        u.regs.dealloc_many([res_reg, free_reg])

class IfStatement:
    def __init__(self, cond, stmt, else_stmt):
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt

    def make_asm(self, symbol_table, code):
        self.cond.make_asm(symbol_table, code)
        else_stmt_lable = asm.Lable('else')
        code.append(asm.B(else_stmt_lable, self.cond.cmp_cmd))
        self.stmt.make_asm(symbol_table, code)

        code.append(else_stmt_lable)
        self.else_stmt.make_asm(symbol_table, code)

class Equals:
    """Expression for assignment."""
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

    def make_asm(self, symbol_table, code):
        if not isinstance(self.left, Identifier):
            raise Exception('Only identifier could be on left side of equal sign')
        l_symbol = symbol_table.lookup(self.left.identifier)
        if not l_symbol:
            raise Exception('Reference before assignment')

        res_reg = self.right.make_asm(symbol_table, code)
        free_reg = u.regs.alloc()
        if l_symbol.scope_type == u.ScopeType.LOCAL:
            cmds = [asm.Minus(free_reg, u.regs.bp, imm=l_symbol.binding), asm.Str(res_reg, free_reg)]
        else:
            cmds = [asm.Mov(free_reg, None, imm=l_symbol.binding), asm.Str(res_reg, free_reg)]

        code.extend(cmds)
        u.regs.dealloc_many([res_reg, free_reg])

### Expressions
class ArithBinOp:
    op_cmd = None

    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def make_asm(self, symbol_table, code):
        l_res_reg = self.left.make_asm(symbol_table, code)
        r_res_reg = self.right.make_asm(symbol_table, code)

        res_reg = u.regs.alloc()
        code.append(self.op_cmd(res_reg, l_res_reg, r_res_reg))
        u.regs.dealloc_many([l_res_reg, r_res_reg])
        return res_reg
    
    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

class Plus(ArithBinOp):
    op_cmd = asm.Add

class Minus(ArithBinOp):
    op_cmd = asm.Minus

class Mul(ArithBinOp):
    op_cmd = asm.Mul

class Relational(ArithBinOp):
    cmp_cmd = None

    def __init__(self, left, right, op):
        super().__init__(left, right, op)

    def make_asm(self, symbol_table, code):
        l_res_reg = self.left.make_asm(symbol_table, code)
        r_res_reg = self.right.make_asm(symbol_table, code)

        code.append(asm.Cmp(None, l_res_reg, r_res_reg))

class LessThan(Relational):
    cmp_cmd = 'lt'

class BiggerThan(Relational):
    cmp_cmd = 'bt'

class Equal(Relational):
    cmp_cmd = 'eq'

class Identifier:
    def __init__(self, identifier):
        self.identifier = identifier
        super().__init__()

    def __repr__(self):
        return f'Identifier: {self.identifier}'

    def make_asm(self, symbol_table: SymbolTable, code):
        symbol = symbol_table.lookup(self.identifier)
        if not symbol:
            raise Exception('Reference before assignment')

        res_reg = u.regs.alloc()
        free_reg = u.regs.alloc()
        if symbol.scope_type == u.ScopeType.LOCAL:
            cmds = [asm.Minus(free_reg, u.regs.bp, imm=symbol.binding), asm.Ldr(res_reg, free_reg)]
        else:
            cmds = [asm.Mov(free_reg, None, imm=symbol.binding), asm.Ldr(res_reg, free_reg)]

        code.extend(cmds)
        u.regs.dealloc(free_reg)

        return res_reg

class Number:
    def __init__(self, number):
        self.number = number
        super().__init__()

    def __repr__(self):
        return f'Number: {self.number}'

    def make_asm(self, symbol_table, code):
        reg = u.regs.alloc()
        code.append(asm.Mov(reg, None, imm=self.number))
        return reg