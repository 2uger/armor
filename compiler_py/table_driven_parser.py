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

    FUNC_DECL = 'FUNC_DECL'
    PARMS = 'PARMS'
    PARMS_LIST = 'PARMS_LIST'
    PARMS_LIST_N = 'PARMS_LIST_N'
    PARM_TYPE = 'PARM_TYPE'

    STMT = 'STMT'
    EXPR_STMT = 'EXPR_STMT'
    COMPOUND_STMT = 'COMPOUND_STMT'
    STMT_LIST = 'STMT_LIST'
    RETURN_STMT = 'RETURN_STMT'

    EXPR = 'EXPR'
    EXPR_N = 'EXPR_N'
    OPERAND = 'OPERAND'


parse_rules = {
    NT.PROGRAM: [[NT.DECL_LIST]],
    NT.DECL_LIST: [[NT.DECL, NT.DECL_LIST]],
    NT.DECL: [[NT.VAR_DECL],
              [NT.FUNC_DECL]],


    NT.VAR_DECL: [[Tokens.VAR, NT.TYPE_SPEC, NT.VAR_DECL_ID, NT.VAR_DECL_INIT, Tokens.SEMICOLON]],
    NT.TYPE_SPEC: [[Tokens.INT],
                   [Tokens.CHAR]],
    NT.VAR_DECL_INIT: [[Tokens.EQUAL_TO, Tokens.IDENTIFIER],
                       []],
    NT.VAR_DECL_ID: [[Tokens.IDENTIFIER]],


    NT.FUNC_DECL: [[Tokens.FUNC, NT.TYPE_SPEC, Tokens.IDENTIFIER, Tokens.L_PAREN, NT.PARMS, Tokens.R_PAREN, NT.STMT]],
    NT.PARMS: [[NT.PARMS_LIST],
               []],
    NT.PARMS_LIST: [[NT.PARM_TYPE, NT.PARMS_LIST_N]],
    NT.PARMS_LIST_N: [[Tokens.COMMA, NT.PARM_TYPE, NT.PARMS_LIST_N],
                      []],
    NT.PARM_TYPE: [[NT.TYPE_SPEC, Tokens.IDENTIFIER]],


    NT.STMT: [[NT.EXPR_STMT],
              [NT.COMPOUND_STMT],
              [NT.RETURN_STMT],
              []],
    NT.EXPR_STMT: [[NT.EXPR, Tokens.SEMICOLON],
                   [Tokens.SEMICOLON]],
    NT.COMPOUND_STMT: [[Tokens.L_CRL_BRCKT, NT.STMT_LIST, Tokens.R_CRL_BRCKT, Tokens.SEMICOLON],
                       []],
    NT.STMT_LIST: [[NT.STMT, NT.STMT_LIST],
                   []],
    NT.RETURN_STMT: [[Tokens.RETURN, NT.EXPR_STMT]],

    
    NT.EXPR: [[Tokens.IDENTIFIER, NT.EXPR_N]],
    NT.EXPR_N: [[Tokens.EQUAL_TO, NT.OPERAND],
                [Tokens.INCREM],
                [Tokens.DECREM],
                []],
    NT.OPERAND: [[Tokens.IDENTIFIER],
                [Tokens.NUMCONST]],

}


parse_table = {
    NT.PROGRAM: {
        Tokens.VAR: parse_rules[NT.PROGRAM][0],
        Tokens.FUNC: parse_rules[NT.PROGRAM][0],
    },
    NT.DECL_LIST: {
        Tokens.VAR: parse_rules[NT.DECL_LIST][0],
        Tokens.FUNC: parse_rules[NT.DECL_LIST][0],
    },
    NT.DECL: {
        Tokens.VAR: parse_rules[NT.DECL][0],
        Tokens.FUNC: parse_rules[NT.DECL][1],
    },

    # Variable declaration
    NT.VAR_DECL: {
        Tokens.VAR: parse_rules[NT.VAR_DECL][0],
    },
    NT.TYPE_SPEC: {
        Tokens.INT: parse_rules[NT.TYPE_SPEC][0],
        Tokens.CHAR: parse_rules[NT.TYPE_SPEC][1],
    },
    NT.VAR_DECL_INIT: {
        Tokens.EQUAL_TO: parse_rules[NT.VAR_DECL_INIT][0],
        Tokens.SEMICOLON: parse_rules[NT.VAR_DECL_INIT][1],
    },
    NT.VAR_DECL_ID: {
        Tokens.IDENTIFIER: parse_rules[NT.VAR_DECL_ID][0],
    },

    # Function declaration
    NT.FUNC_DECL: {
        Tokens.FUNC: parse_rules[NT.FUNC_DECL][0],
    },
    NT.PARMS: {
        Tokens.INT: parse_rules[NT.PARMS][0],
        Tokens.CHAR: parse_rules[NT.PARMS][0],
        Tokens.R_PAREN: parse_rules[NT.PARMS][1],
    },
    NT.PARMS_LIST: {
        Tokens.INT: parse_rules[NT.PARMS_LIST][0],
        Tokens.CHAR: parse_rules[NT.PARMS_LIST][0],
    },
    NT.PARMS_LIST_N: {
        Tokens.COMMA: parse_rules[NT.PARMS_LIST_N][0],
        Tokens.R_PAREN: parse_rules[NT.PARMS_LIST_N][1],
    },
    NT.PARM_TYPE: {
        Tokens.INT: parse_rules[NT.PARM_TYPE][0],
        Tokens.CHAR: parse_rules[NT.PARM_TYPE][0],
    },
    NT.STMT: {
        Tokens.IDENTIFIER: parse_rules[NT.STMT][0],
        Tokens.L_CRL_BRCKT: parse_rules[NT.STMT][1],
        Tokens.RETURN: parse_rules[NT.STMT][2],
    },
    NT.EXPR_STMT: {
        Tokens.IDENTIFIER: parse_rules[NT.EXPR_STMT][0],
        Tokens.SEMICOLON: parse_rules[NT.EXPR_STMT][1],
    },
    NT.COMPOUND_STMT: {
        Tokens.L_CRL_BRCKT: parse_rules[NT.COMPOUND_STMT][0],
    },
    NT.STMT_LIST: {
        Tokens.IDENTIFIER: parse_rules[NT.STMT_LIST][0],
        Tokens.L_CRL_BRCKT: parse_rules[NT.STMT_LIST][0],
        Tokens.RETURN: parse_rules[NT.STMT_LIST][0],
        Tokens.R_CRL_BRCKT: parse_rules[NT.STMT_LIST][1],
    },
    NT.RETURN_STMT: {
        Tokens.RETURN: parse_rules[NT.RETURN_STMT][0],
    },
    NT.EXPR: {
        Tokens.IDENTIFIER: parse_rules[NT.EXPR][0],
    },
    NT.EXPR_N: {
        Tokens.EQUAL_TO: parse_rules[NT.EXPR_N][0],
        Tokens.INCREM: parse_rules[NT.EXPR_N][1],
        Tokens.DECREM: parse_rules[NT.EXPR_N][2],
        Tokens.SEMICOLON: parse_rules[NT.EXPR_N][3],
    },
    NT.OPERAND: {
        Tokens.IDENTIFIER: parse_rules[NT.OPERAND][0],
        Tokens.NUMCONST: parse_rules[NT.OPERAND][1],
    },
}


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


def parse(tokens):
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
            print(f'Current tokens: {tokens}\nCurrent stack: {stack}')
            return

        parse_rule = parse_table.get(stack.top_elem, {}).get(token)
        if parse_rule is not None:
            stack.pop()

            for s in parse_rule[::-1]:
                stack.push(s)
            continue
        print(f'ERROR: current token {token} does not match at all')
        print(f'Current tokens: {tokens}\nCurrent stack: {stack}')
        return
    print('MSG: program successfully parsed')
    return
