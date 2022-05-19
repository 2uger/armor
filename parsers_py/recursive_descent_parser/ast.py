class AstNode:
    pass

class VarDef(AstNode):
    def __init__(self, var, bin_op, expr):
        self.var = var
        self.bin_op = bin_op
        self.expr = expr

class ExprBinOp(AstNode):
    def __init__(self, bin_op, l_expr, r_expr):
        self.bin_op = bin_op
        self.l_expr = l_expr
        self.r_expr = r_expr
