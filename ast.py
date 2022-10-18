import ir
import utils
import asm

from symbol_table import CType, CTypeInt, NewSymbolTable, SymbolTable
from context import Context


class EmptyNode:
    """Empty node."""
    def make_asm(self, *args, **kwargs):
        pass
    def make_ir(self, *args, **kwargs):
        pass

class Program:
    """Main node of the whole program."""
    def __init__(self, nodes):
        self.nodes = nodes

    def make_asm(self, symbol_table, code, ctx):
        for n in self.nodes:
            n.make_asm(symbol_table, code, ctx)

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

    def make_ir(self, symbol_table: NewSymbolTable, ir_gen: ir.IRGen, ctx):
        spec = self.node.spec
        decl = self.node.decl
        init = self.node.init
        if isinstance(decl, Identifier):
            var = symbol_table.add_variable(decl.identifier)
            out = init.make_ir(symbol_table, ir_gen, ctx)
            if not ctx.is_global:
                ir_gen.register_variable(var)
                ir_gen.add(ir.Set(var, out))
        elif isinstance(decl, Function):
            ctx.set_global(False)
            ir_gen.new_func(decl.identifier.identifier)
            ir_gen.add(ir.Lable(decl.identifier.identifier))
            for parm in decl.parms:
                var = symbol_table.add_variable(parm.decl.identifier)
                ir_gen.register_variable(var)
            self.body.make_ir(symbol_table, ir_gen, ctx)

    def make_asm(self, symbol_table: SymbolTable, code, ctx: Context):
        spec = self.node.spec
        decl = self.node.decl
        init = self.node.init

        if isinstance(decl, Identifier):
            symbol = symbol_table.lookup(decl.identifier)
            if symbol:
                if ctx.is_global:
                    raise Exception(f'multiple declaration for: {decl.identifier}')
                # Already declared variable inside function, just need to generate code for them
                elif not ctx.is_global and init:
                    res_reg = init.make_asm(symbol_table, code, ctx)
                    free_reg = utils.regs.alloc()
                    code.extend([asm.Add(free_reg, utils.regs.bp, imm=symbol.binding), asm.Str(res_reg, free_reg)])
                    utils.regs.dealloc_many([res_reg, free_reg])
                    return

            c_type = utils.get_c_type(spec)
            # global variable declaration - it's located in static storage
            mem_binding = utils.static_storage.place(None, c_type.size)
            symbol_table.add_symbol(decl.identifier, c_type, mem_binding, utils.ScopeType.GLOBAL)

            # Global variable - compile time computation
            if ctx.is_global:
                if init:
                    res_value = init.make_asm(symbol_table, code, ctx)
                    code.insert(0, asm.Data(mem_binding, decl.identifier, res_value))
                else:
                    code.insert(0, asm.Data(mem_binding, decl.identifier, 0x0))
            elif not ctx.is_global and init:
                res_reg = init.make_asm(symbol_table, code, ctx)
                free_reg = utils.regs.alloc()
                code.extend([asm.Mov(free_reg, None, imm=mem_binding), asm.Str(res_reg, free_reg)])
                utils.regs.dealloc_many([res_reg, free_reg])
        elif isinstance(decl, Function):
            return_c_type = utils.get_c_type(spec)
            ctx.set_return(return_c_type)
            ctx.set_global(False)

            """
            Stack for function call:

            arg-1
            arg-2
            ...
            arg-n
            ret_value
            ret_address
            old_bp <- BP - frame pointer for current function points here
            """

            parms_total_size = 0
            parms_list = []
            for parm in decl.parms:
                c_type = utils.get_c_type(parm.spec)
                # Collect parms list for function slot in symbol table
                parms_list.append((c_type, parm.decl.identifier))
                parms_total_size += c_type.size
                
            symbol_table.add_symbol(decl.identifier.identifier, return_c_type, None, utils.ScopeType.GLOBAL, parms_list)
            symbol_table.open_scope()

            # bp offset for first argument = old_bp + red_address + ret_value + len(args) * size
            bp_offset = 4 + 4 + return_c_type.size + parms_total_size

            for parm in decl.parms:
                c_type = utils.get_c_type(parm.spec)
                # Place parameter itself into symbol table
                symbol_table.add_symbol(parm.decl.identifier, c_type, bp_offset, utils.ScopeType.LOCAL)
                bp_offset -= c_type.size

            # Collect local variables to create empty space on stack for them
            local_var_bp_offset = 0
            for stmt in utils.nested_traverse(self.body):
                if isinstance(stmt, Declaration):
                    c_type = utils.get_c_type(stmt.node.spec)
                    local_var_bp_offset += c_type.size
                    symbol_table.add_symbol(stmt.node.decl.identifier, c_type, local_var_bp_offset, utils.ScopeType.LOCAL)

            code.extend([asm.Lable(decl.identifier.identifier),
                         asm.Push([utils.regs.bp]),
                         asm.Mov(utils.regs.bp, utils.regs.sp),
                         asm.Sub(utils.regs.sp, utils.regs.sp, imm=local_var_bp_offset)],)
            self.body.make_asm(symbol_table, code, ctx)

            symbol_table.close_scope()
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
    def __init__(self, func, args):
        self.func = func
        self.args = args

    def make_ir(self, symbol_table, ir_gen, ctx):
        if ctx.is_global:
            raise Exception(f'can\'t call function in global context')
        args = [arg.make_ir(symbol_table, ir_gen, ctx) for arg in self.args]
        out = ir.IRValue(CTypeInt)
        ir_gen.add(ir.FuncCall(out, self.func, args))

        return out

    def make_asm(self, symbol_table: SymbolTable, code, ctx):
        if ctx.is_global:
            raise Exception(f'can\'t call function in global context')
        # Check that function we call exists and parameters amount is equal to args amount
        func_symbol = symbol_table.lookup(self.func)
        if func_symbol:

            # TODO: check also arguments and parameters types, when add new type

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
            utils.regs.dealloc_many(args_to_push)
        
        return_value_size = func_symbol.c_type.size
        # Push space for return value
        code.append(asm.Add(utils.regs.sp, utils.regs.sp, imm=return_value_size))
        # Call function
        code.append(asm.BL(self.func))

        # Collect return value
        # Clean up stack from arguments
        # TODO: count total arguments size, when use new type
        free_reg = utils.regs.alloc()
        res_reg = utils.regs.alloc()
        code.extend([asm.Sub(free_reg, utils.regs.sp, imm=4 * 2),
                     asm.Str(res_reg, free_reg),
                     asm.Sub(utils.regs.sp, utils.regs.sp, imm=4 * 3 + utils.CTypeInt.size * len(args_to_push))])
        utils.regs.dealloc(free_reg)

        return res_reg

class ExprStmt:
    """Single Expression statement."""
    def __init__(self, expr):
        self.expr = expr

class Compound:
    """List of expressions."""
    def __init__(self, items):
        self.items = items

    def make_asm(self, symbol_table, code, ctx):
        for item in self.items:
            item.make_asm(symbol_table, code, ctx)

    def make_ir(self, symbol_table, ir_gen, ctx):
        for item in self.items:
            item.make_ir(symbol_table, ir_gen, ctx)

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

    def __len__(self):
        return len(self.items)

    def __getitem__(self, n):
        return self.items[n]

class Return:
    def __init__(self, ret_expr):
        self.ret_expr = ret_expr

    def __repr__(self):
        return f'return {self.ret_expr}'

    def make_ir(self, symbol_table, ir_gen, ctx):
        out = self.ret_expr.make_ir(symbol_table, ir_gen, ctx)
        ir_gen.add(ir.Return(out))

    def make_asm(self, symbol_table, code, ctx):
        if ctx.return_type == utils.CTypeVoid:
            raise Exception(f'void function should not return: {self.ret_expr}')
        res_reg = self.ret_expr.make_asm(symbol_table, code, ctx)
        free_reg = utils.regs.alloc()
        # Store result in return value place
        code.extend([asm.Sub(free_reg, utils.regs.bp, imm=4 * 2),
                     asm.Str(res_reg, free_reg)])
        # Return from function
        code.extend([asm.Mov(utils.regs.sp, utils.regs.bp),
                     asm.Pop([utils.regs.bp, free_reg]),
                     asm.BX(free_reg)])
        utils.regs.dealloc_many([res_reg, free_reg])

class IfStatement:
    def __init__(self, cond, stmt, else_stmt):
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt

    def make_ir(self, symbol_table, ir_gen, ctx):
        pass

    def make_asm(self, symbol_table, code, ctx):
        if not isinstance(self.cond, Relational):
            # Everything is True inside if statement if result of expression > 0
            res_reg = self.cond.make_asm(symbol_table, code, ctx)
            free_reg = utils.regs.alloc()
            code.extend([asm.Mov(free_reg, None, imm=0),
                         asm.Cmp(None, res_reg, free_reg)])
            utils.regs.dealloc_many([res_reg, free_reg])
            cmp_cmd = 'gt'
        else:
            self.cond.make_asm(symbol_table, code, ctx)
            cmp_cmd = self.cond.cmp_cmd

        else_stmt_lable = utils.lable.get()
        code.append(asm.B(else_stmt_lable, cmp_cmd))
        self.stmt.make_asm(symbol_table, code, ctx)

        code.append(asm.Lable(else_stmt_lable))
        self.else_stmt.make_asm(symbol_table, code, ctx)

    def __iter__(self):
        self.if_n = 0
        self.else_n = 0
        return self

    def __next__(self):
        """To be able to iterate through all expressions."""
        while self.if_n < len(self.stmt):
            item = self.stmt[self.if_n]
            self.if_n += 1
            return item
        while self.else_n < len(self.else_stmt):
            item = self.else_stmt[self.else_n]
            self.else_n += 1
            return item
        raise StopIteration

class Equals:
    """Expression for assignment."""
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

    def make_ir(self, symbol_table, ir_gen, ctx):
        dst = symbol_table.lookup(self.left.identifier)
        res = self.right.make_ir(symbol_table, ir_gen, ctx)
        ir_gen.add(ir.Set(dst, res))

    def make_asm(self, symbol_table, code, ctx):
        if not isinstance(self.left, Identifier):
            raise Exception('only identifier could be on left side of equal sign')
        l_symbol = symbol_table.lookup(self.left.identifier)
        if not l_symbol:
            raise Exception(f'reference before assignment: {self.left.identifier}')

        res_reg = self.right.make_asm(symbol_table, code, ctx)
        free_reg = utils.regs.alloc()
        if l_symbol.scope_type == utils.ScopeType.LOCAL:
            cmds = [asm.Sub(free_reg, utils.regs.bp, imm=l_symbol.binding),
                    asm.Str(res_reg, free_reg)]
        else:
            cmds = [asm.Mov(free_reg, None, imm=l_symbol.binding),
                    asm.Str(res_reg, free_reg)]

        code.extend(cmds)
        utils.regs.dealloc_many([res_reg, free_reg])

### Expressions
class ArithBinOp:
    op_cmd = None
    ir_cmd = None

    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def make_ir(self, symbol_table, ir_gen, ctx):
        """Create ir code for binary operation."""
        out = ir.IRValue(CTypeInt)
        l_val = self.left.make_ir(symbol_table, ir_gen, ctx)
        r_val = self.right.make_ir(symbol_table, ir_gen, ctx)
        ir_cmd = self.ir_cmd(out, l_val, r_val)
        ir_gen.add(ir_cmd)

        return out

    def make_asm(self, symbol_table, code, ctx):
        # As soon as in global scope all variables should be computed at compile time
        # otherwise we will raise exception
        if ctx.is_global:
            l_val = self.left.make_asm(symbol_table, code, ctx)
            r_val = self.right.make_asm(symbol_table, code, ctx)
            operation = {
                asm.Add: lambda x, y: x + y,
                asm.Sub: lambda x, y: x - y,
                asm.Mul: lambda x, y: x * y
            }.get(self.op_cmd)
            return operation(l_val, r_val)
        else:
            l_res_reg = self.left.make_asm(symbol_table, code, ctx)
            r_res_reg = self.right.make_asm(symbol_table, code, ctx)

            res_reg = utils.regs.alloc()
            code.append(self.op_cmd(res_reg, l_res_reg, r_res_reg))
            utils.regs.dealloc_many([l_res_reg, r_res_reg])
            return res_reg
    
    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

class Plus(ArithBinOp):
    op_cmd = asm.Add
    ir_cmd = ir.Add

class Minus(ArithBinOp):
    op_cmd = asm.Sub
    ir_cmd = ir.Sub

class Mul(ArithBinOp):
    op_cmd = asm.Mul
    ir_cmd = ir.Mul

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
    cmp_cmd = 'gt'

class Equal(Relational):
    cmp_cmd = 'eq'

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

    def make_asm(self, symbol_table: SymbolTable, code, ctx):
        if ctx.is_global:
            # Variable couldn't be used as right value in global scope
            raise Exception('can\'t use variable in global scope as right value')
        symbol = symbol_table.lookup(self.identifier)
        if not symbol:
            raise Exception(f'reference before assignment: {self.identifier}')

        res_reg = utils.regs.alloc()
        free_reg = utils.regs.alloc()
        if symbol.scope_type == utils.ScopeType.LOCAL:
            cmds = [asm.Sub(free_reg, utils.regs.bp, imm=symbol.binding), asm.Ldr(res_reg, free_reg)]
        else:
            cmds = [asm.Mov(free_reg, None, imm=symbol.binding), asm.Ldr(res_reg, free_reg)]

        code.extend(cmds)
        utils.regs.dealloc(free_reg)

        return res_reg

class Number:
    def __init__(self, number):
        self.number = number

    def __repr__(self):
        return f'Number: {self.number}'

    def make_ir(self, symbol_table, ir_gen, ctx):
        return ir.IRValue(CTypeInt, self.number)

    def make_asm(self, symbol_table, code, ctx):
        if self.number >= 2 ** (32):
            raise Exception('can handle only 4 bytes integer')
        if ctx.is_global:
            return self.number
        reg = utils.regs.alloc()
        code.append(asm.Mov(reg, None, imm=self.number))
        return reg
