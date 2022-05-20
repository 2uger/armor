class Regs:
    def __init__(self):
        self.free_regs = [r for r in range(15)]

    def allocate(self):
        return self.free_reg.pop()

    def put(self, reg):
        reg + self.free_reg

    def put_many(self, regs):
        regs + self.free_reg
        
