class IRGen:
    """Intermediate representation state."""

    def __init__(self):
        self.curr_func = None
        self.cmds = {}
        # function arguments
        self.func_args = {}
        # function locals
        self.func_locals = {}
        self.globals = {} 

    def add(self, ir_cmd):
        self.cmds[self.curr_func].append(ir_cmd)
    
    def new_func(self, func_name):
        self.curr_func = func_name
        self.cmds[func_name] = []

        self.func_args[func_name] = []
        self.func_locals[func_name] = []

    def register_argument(self, var):
        """Register function argument."""
        self.func_args[self.curr_func].append(var)

    def register_local(self, var):
        """Register function local variable."""
        self.func_locals[self.curr_func].append(var)

    def register_global(self, var, value):
        """Register global variable."""
        self.globals[var] = value
