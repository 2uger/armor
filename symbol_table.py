from collections import namedtuple
import ir

"""
binding: memory location(absolute for GLOBAL, relative by BP register for LOCAL)
parms_list: list of parameters for function, None for variable
"""
Symbol = namedtuple('Symbol', ['name', 'c_type', 'size', 'binding', 'scope_type', 'parms_list'])

class SymbolTable:
    def __init__(self):
        self.tables = []

        self.open_scope()

    def add_variable(self, name, c_type, bin):
        pass

    def add_symbol(self, name, c_type, binding, scope_type, parms_list=None):
        curr_table = self.tables[len(self.tables) - 1]
        curr_table[name] = Symbol(name, c_type, c_type.size, binding, scope_type, parms_list)
    
    def lookup(self, name):
        symbol = None
        for table in self.tables[::-1]:
            if name in table:
                symbol = table[name]
                break
        return symbol

    def open_scope(self):
        self.tables.append({})
    
    def close_scope(self):
        self.tables.pop()

# Describe C types
CType = namedtuple('CType', ['size'])
CTypeInt = CType(4)
CTypeChar = CType(2)
CTypeVoid = CType(0)

class NewSymbolTable:

    def __init__(self):
        self.tables = []

        self.open_scope()

    def add_variable(self, name, c_type=CTypeInt):
        curr_table = self.tables[len(self.tables) - 1]
        var = ir.IRValue(name)
        curr_table[name] = var
        return var
    
    def lookup(self, name):
        var = None
        for table in self.tables[::-1]:
            if name in table:
                var = table[name]
                break
        return var

    def open_scope(self):
        self.tables.append({})
    
    def close_scope(self):
        self.tables.pop()