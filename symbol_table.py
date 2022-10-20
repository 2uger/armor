import enum
from collections import namedtuple
import ir

class ScopeType(enum.Enum):
    """
    Would help to differentiate local and global variables memory binding.
    Because global vars will have absolute memory address, while local would
    have offset from BP register.
    """
    LOCAL = 'local'
    GLOBAL = 'global'

# Describe C types
CType = namedtuple('CType', ['size'])
CTypeInt = CType(4)
CTypeChar = CType(2)
CTypeVoid = CType(0)

class NewSymbolTable:

    def __init__(self):
        self.tables = []

        self.open_scope()

    def add_variable(self, name, c_type=CTypeInt, binding=ScopeType.LOCAL):
        """Add variable to symbol table."""
        curr_table = self.tables[len(self.tables) - 1]
        var = ir.IRValue(c_type, name)
        curr_table[name] = (var, binding)
        return var
    
    def lookup(self, name):
        var = None
        for table in self.tables[::-1]:
            if name in table:
                (var, _) = table[name]
                break
        return var
    
    def open_scope(self):
        self.tables.append({})
    
    def close_scope(self):
        self.tables.pop()