from symbol_table import SymbolTable
from context import Context
import utils as u
import asm as asm
import tokens as t


class EmptyNode:
    """Empty node."""
    def make_asm(self, *args, **kwargs):
        pass

class Program:
    """Main node of the whole program."""
    def __init__(self, nodes):
        self.nodes = nodes

    def make_asm(self, symbol_table, code, ctx):
        for n in self.nodes:
            n.make_asm(symbol_table, code, ctx)

class Declaration:
    def __init__(self, node, body=None):
        """
        body - only for functions
        """
        self.node = node
        self.body = body
    
    def __repr__(self):
        return f'{self.node}: {self.body}'

    def make_asm(self, symbol_table: SymbolTable, code, ctx: Context):
        spec = self.node.spec
        decl = self.node.decl
        init = self.node.init

        if isinstance(decl, Identifier):
            symbol = symbol_table.lookup(decl.identifier)
            if symbol:
                if ctx.is_global:
                    raise Exception(f'Multiple declaration for: {decl.identifier}')
                # Already declared variable inside function
                elif not ctx.is_global and init:
                    res_reg = init.make_asm(symbol_table, code, ctx)
                    free_reg = u.regs.alloc()
                    code.extend([asm.Add(free_reg, u.regs.bp, imm=symbol.binding), asm.Str(res_reg, free_reg)])
                    u.regs.dealloc(res_reg)
                    return

            # TODO: after add new types - change this
            c_type = u.CTypeInt

            # global variable declaration - it's located in static storage
            mem_binding = u.static_storage.place(None, c_type.size)
            # local variable declaration - should be places on stack, when step into function
                
            if init:
                res_reg = init.make_asm(symbol_table, code, ctx)
                free_reg = u.regs.alloc()
                code.extend([asm.Mov(free_reg, None, imm=mem_binding), asm.Str(res_reg, free_reg)])
                u.regs.dealloc_many([res_reg, free_reg])
            symbol_table.add_symbol(decl.identifier, c_type, mem_binding, u.ScopeType.GLOBAL)
        elif isinstance(decl, Function):
            # TODO: after add new types - change this
            return_c_type = {t.TokenKind.VOID: u.CTypeVoid,
                             t.TokenKind.INT: u.CTypeInt}.get(spec.kind)
            ctx.set_return(return_c_type)
            ctx.set_global(False)

            # Collect function parameters
            parms_list = []
            for parm in decl.parms:
                # TODO: after add new types - change this
                c_type = u.CTypeInt
                parm_name = parm.decl.identifier
                parms_list.append((c_type, parm_name))
            symbol_table.add_symbol(decl.identifier.identifier, return_c_type, None, u.ScopeType.GLOBAL, parms_list)
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

            # bp offset for first argument = old_bp + red_address + ret_value + len(args) * size
            bp_offset = 4 + 4 + 4 + u.CTypeInt.size * len(decl.parms)

            for parm in decl.parms:
                parm_name = parm.decl.identifier
                c_type = u.CTypeInt
                symbol_table.add_symbol(parm_name, c_type, bp_offset, u.ScopeType.LOCAL)
                bp_offset -= c_type.size

            # Collect local variables to create empty space on stack for them
            local_var_bp_offset = 0
            for stmt in self.body:
                if isinstance(stmt, Declaration):
                    _decl = stmt.node.decl
                    # TODO: after add new types - change this
                    c_type = u.CTypeInt
                    symbol_table.add_symbol(_decl.identifier, c_type, local_var_bp_offset, u.ScopeType.LOCAL)
                    local_var_bp_offset += c_type.size

            code.extend(['\n', asm.Lable(decl.identifier),
                         asm.Push([u.regs.bp]),
                         asm.Mov(u.regs.bp, u.regs.sp),
                         asm.Sub(u.regs.sp, u.regs.sp, imm=local_var_bp_offset)],)
            self.body.make_asm(symbol_table, code, ctx)

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
    def __init__(self, identifier, parms):
        self.identifier = identifier
        self.parms = parms

    def __repr__(self):
        return f'{self.identifier}: {self.parms}'

class FuncCall:
    """Represents function call."""
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def make_asm(self, symbol_table: SymbolTable, code, ctx):
        # Check that function we call exists
        # and parameters amount is equal to args amount
        func_symbol = symbol_table.lookup(self.func)
        if func_symbol:
            if not len(self.args) == len(func_symbol.parms_list):
                raise Exception(f'wrong amount of arguments to function: {func_symbol.name}')
        else:
            raise Exception(f'function you call: {self.func} does not exists')

        # TODO: save registers in use, because it might store local variables

        args_to_push = []

        for arg in self.args:
            args_to_push.append(arg.make_asm(symbol_table, code, ctx))

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
        code.extend([asm.Sub(free_reg, u.regs.sp, imm=u.CTypeInt.size), asm.Str(res_reg, free_reg)])
        u.regs.dealloc(free_reg)

        # Clean up stack from arguments
        code.append(asm.Sub(u.regs.sp, u.regs.sp, imm=4 * 2 + u.CTypeInt.size * len(args_to_push)))

        return res_reg

class ExprStmt:
    """Single Expression statement."""
    def __init__(self, expr):
        self.expr = expr

class Compound:
    def __init__(self, items):
        self.items = items

    def make_asm(self, symbol_table, code, ctx):
        for item in self.items:
            print(item)
            item.make_asm(symbol_table, code, ctx)

    def __repr__(self):
        r = '{\n'
        for i in self.items:
            r += str(i) + '\n'
        r += '}'
        return r

    def __iter__(self):
        self.n = 0
        return self

    def __next__(self):
        while self.n < len(self.items):
            item = self.items[self.n]
            self.n += 1
            return item
        raise StopIteration

class Return:
    def __init__(self, ret_expr):
        self.ret_expr = ret_expr

    def __repr__(self):
        return f'return {self.ret_expr}'

    def make_asm(self, symbol_table, code, ctx):
        if ctx.return_type == u.CTypeVoid:
            raise Exception(f'Void function should not return: {self.ret_expr}')
        res_reg = self.ret_expr.make_asm(symbol_table, code, ctx)
        free_reg = u.regs.alloc()
        # Store result in return value place
        code.extend([asm.Sub(free_reg, u.regs.bp, imm=u.CTypeInt.size * 2), asm.Str(res_reg, free_reg)])
        # Return from function
        code.extend([asm.Pop([u.regs.bp, free_reg]), asm.BX(free_reg)])
        u.regs.dealloc_many([res_reg, free_reg])

class IfStatement:
    def __init__(self, cond, stmt, else_stmt):
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt

    def make_asm(self, symbol_table, code, ctx):
        if not isinstance(self.cond, Relational):
            raise Exception('Can handle only relational conditional inside if statement')

        self.cond.make_asm(symbol_table, code, ctx)

        else_stmt_lable = u.lable.get()
        code.append(asm.B(else_stmt_lable, self.cond.cmp_cmd))
        self.stmt.make_asm(symbol_table, code, ctx)

        code.append(asm.Lable(else_stmt_lable))
        self.else_stmt.make_asm(symbol_table, code, ctx)

class Equals:
    """Expression for assignment."""
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

    def make_asm(self, symbol_table, code, ctx):
        if not isinstance(self.left, Identifier):
            raise Exception('Only identifier could be on left side of equal sign')
        l_symbol = symbol_table.lookup(self.left.identifier)
        if not l_symbol:
            raise Exception('Reference before assignment')

        res_reg = self.right.make_asm(symbol_table, code, ctx)
        free_reg = u.regs.alloc()
        if l_symbol.scope_type == u.ScopeType.LOCAL:
            cmds = [asm.Sub(free_reg, u.regs.bp, imm=l_symbol.binding), asm.Str(res_reg, free_reg)]
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

    def make_asm(self, symbol_table, code, ctx):
        l_res_reg = self.left.make_asm(symbol_table, code, ctx)
        r_res_reg = self.right.make_asm(symbol_table, code, ctx)

        res_reg = u.regs.alloc()
        code.append(self.op_cmd(res_reg, l_res_reg, r_res_reg))
        u.regs.dealloc_many([l_res_reg, r_res_reg])
        return res_reg
    
    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

class Plus(ArithBinOp):
    op_cmd = asm.Add

class Minus(ArithBinOp):
    op_cmd = asm.Sub

class Mul(ArithBinOp):
    op_cmd = asm.Mul

class Relational(ArithBinOp):
    cmp_cmd = None

    def __init__(self, left, right, op):
        super().__init__(left, right, op)

    def make_asm(self, symbol_table, code, ctx):
        l_res_reg = self.left.make_asm(symbol_table, code, ctx)
        r_res_reg = self.right.make_asm(symbol_table, code, ctx)

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

    def make_asm(self, symbol_table: SymbolTable, code, ctx):
        symbol = symbol_table.lookup(self.identifier)
        if not symbol:
            raise Exception('Reference before assignment')

        res_reg = u.regs.alloc()
        free_reg = u.regs.alloc()
        if symbol.scope_type == u.ScopeType.LOCAL:
            cmds = [asm.Sub(free_reg, u.regs.bp, imm=symbol.binding), asm.Ldr(res_reg, free_reg)]
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

    def make_asm(self, symbol_table, code, ctx):
        reg = u.regs.alloc()
        code.append(asm.Mov(reg, None, imm=self.number))
        return reg