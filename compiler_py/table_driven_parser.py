from tokens import *

class NT:
    END = 'end'
    PROGRAM = 'PROGRAM'
    DECL_LIST = 'DECL_LIST'
    DECL = 'DECL'

    VAR_DECL = 'VAR_DECL'
    TYPE_SPEC = 'TYPE_SPEC'
    VAR_DECL_INIT = 'VAR_DECL_INIT'
    VAR_DECL_ID = 'VAR_DECL_ID'

    EXPR = 'EXPR'
    SIMPLE_EXPR = 'SIMPLE_EXPR'


first_set = {
    NT.PROGRAM: set([Tokens.VAR, Tokens.EMPTY]),
    NT.DECL_LIST: set([Tokens.VAR, Tokens.EMPTY]),
    NT.DECL: set([Tokens.VAR]),

    NT.VAR_DECL: set([Tokens.VAR]),
    NT.TYPE_SPEC: set([Tokens.INT, Tokens.CHAR, Tokens.BOOL]),
    NT.VAR_DECL_INIT: set([Tokens.IDENTIFIER]),
    NT.VAR_DECL_ID: set([Tokens.IDENTIFIER]),

    NT.SIMPLE_EXPR: set([Tokens.IDENTIFIER]),
    NT.EXPR: set([Tokens.IDENTIFIER]),
}

tokens = []

class Stack:
    def __init__(self):
        self.stack = []

    def push(self, e):
        self.stack.insert(0, e)

    def pop(self):
        if self.stack:
            return self.stack.pop(0)
        return None

    @property
    def top_elem(self):
        if self.stack:
            return self.stack[0]
        return None

    def __repr__(self):
        return str(self.stack)

parse_table = {
    NT.PROGRAM: {
        Tokens.VAR: 1,
    },
    NT.DECL_LIST: {
        Tokens.VAR: 2,
    },
    NT.DECL: {
        Tokens.VAR: 3,
    },
    NT.VAR_DECL: {
        Tokens.VAR: 4,
    },
    NT.TYPE_SPEC: {
        Tokens.INT: 5,
        Tokens.CHAR: 6,
    },
    NT.VAR_DECL_INIT: {
        Tokens.IDENTIFIER: 7,
    },
    NT.VAR_DECL_ID: {
        Tokens.IDENTIFIER: 8,
    },
}

parse_rules = {
    1: [NT.DECL_LIST],
    2: [NT.DECL, NT.DECL_LIST],
    3: [NT.VAR_DECL],

    4: [Tokens.VAR, NT.TYPE_SPEC, NT.VAR_DECL_INIT, Tokens.SEMICOLON],
    5: [Tokens.INT],
    6: [Tokens.CHAR],
    7: [NT.VAR_DECL_ID, Tokens.EQUAL_TO, Tokens.IDENTIFIER],
    8: [Tokens.IDENTIFIER],
}

def parser(tokens):
    stack = Stack()
    stack.push(NT.END)
    stack.push(NT.PROGRAM)

    token = scan_token(tokens)
    if not token:
        print('INFO: empty input')
        return

    while stack:
        if stack.top_elem == NT.END:
            break
        if stack.top_elem == token:
            stack.pop()

            token = scan_token(tokens)
            if not token:
                print('MSG: input of tokens is empty')
                break

            continue
        if type(stack.top_elem) == Tokens and token != stack.top_elem:
            print(f'ERROR: current token {token} and stack top element {stack.top_elem} does not match')
            return
        parse_rule_num = parse_table.get(stack.top_elem).get(token)
        if parse_rule_num:
            parse_rule = parse_rules.get(parse_rule_num)

            stack.pop()

            for s in parse_rule[::-1]:
                stack.push(s)
            continue
    print('MSG: program successfully parsed')
    return

tokens = [Tokens.VAR, Tokens.INT, Tokens.IDENTIFIER, Tokens.EQUAL_TO, Tokens.IDENTIFIER]
        
parser(tokens)

