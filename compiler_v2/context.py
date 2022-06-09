class Context:
    """Represent context of current execution

    return_type - return type for function
    is_global - wheter the current scope is global or within function
    """
    def __init__(self):
        self.return_type = None
        self.is_global = False

    def set_global(self, val):
        self.is_global = val

    def set_return(self, c_type):
        self.return_type = c_type
