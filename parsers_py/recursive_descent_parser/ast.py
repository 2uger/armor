class Node:
    pass

class Root(Node):
    def __init__(self, spec, decl, init=None):
        """
        Param:
        spec - specifier like int, char
        decl - name of the variable
        init - initial value
        """
        self.spec = spec
        self.decl = decl
        self.init = init

    def __repr__(self):
        return f'{self.spec} {self.decl} = {self.init}'

class Declaration(Node):
    def __init__(self, node, body=None):
        self.node = node
        self.body = body
    
    def __repr__(self):
        return f'{self.node}: {self.body}'

class Function(Node):
    def __init__(self, identifier, args):
        self.identifier = identifier
        self.args = args

    def __repr__(self):
        return f'{self.identifier}: {self.args}'

class Identifier(Node):
    def __init__(self, identifier):
        self.identifier = identifier
        super().__init__()

    def __repr__(self):
        return f'Identifier: {self.identifier}'

class Number(Node):
    def __init__(self, number):
        self.number = number
        super().__init__()

    def __repr__(self):
        return f'Number: {self.number}'

### Expressions
class ArithBinOp(Node):
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def make_asm(self, symbol_table, code, reg_table):
        if isinstance(self.left, self):
            l_res_reg = self.left.make_asm(symbol_table, code)
        elif type(self.left) == Number:
            l_res_reg = reg_table.allocate()
            code.append(f'mov r{r_res_reg}, #{self.left.value}')
        elif type(self.left) == Identifier:
            symbol = symbol_table.get(self.left.name)
        
        if isinstance(self.right, self):
            r_res_reg = self.left_make_asm(symbol_table, code)
        elif type(self.right) == Number:
            r_res_reg = reg_table.allocate()
            code.append(f'mov r{r_res_reg}, #{self.right.value}')
        elif type(self.right) == Identifier:
            symbol = symbol_table.get(self.right.name)


        res_reg = reg_table.get_free_reg()
        code.append(f'add r{res_reg}, r{l_res_reg}, r{r_res_reg}')
        reg_table.put_many([l_res_reg, r_res_reg, res_reg])
    
    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'

class Plus(ArithBinOp):
    def __init__(self, left, right, op):
        super().__init__(left, right, op)
    
class Minus(ArithBinOp):
    def __init__(self, left, right, op):
        super().__init__(left, right, op)

class FuncCall(Node):
    """Represents function call."""
    def __init__(self, func, args):
        self.func = func
        self.args = args

### Statements
class ExprStmt(Node):
    """Single Expression statement."""
    def __init__(self, expr):
        self.expr = expr

class Compound(Node):
    def __init__(self, items):
        self.items = items

    def __repr__(self):
        r = '{\n'
        for i in self.items:
            r += str(i) + '\n'
        r += '}'
        return r

class Return(Node):
    def __init__(self, ret_value):
        self.ret_value = ret_value

    def __repr__(self):
        return f'return {self.ret_value}'

class IfStatement(Node):
    def __init__(self, cond, stmt, else_stmt):
        self.cond = cond
        self.stmt = stmt
        self.else_stmt = else_stmt

class Equals(Node):
    """Expression for assignment."""
    def __init__(self, left, right, op):
        self.left = left
        self.right = right
        self.op = op

    def __repr__(self):
        return f'{self.left} {self.op} {self.right}'