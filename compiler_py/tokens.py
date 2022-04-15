import enum
import tokenize

class NT():
    PROGRAM = 'program'
    DECL = 'DECL'
    VAR_DECL = 'VAR_DECL'
    FUNC_DECL = 'FUNC_DECL'
    TYPE_SPEC = 'TYPE_SPEC'
    VAR_DECL = 'VAR_DECL'
    VAR_DECL_INIT = 'VAR_DECL_INIT'
    VAR_DECL_ID = 'VAR_DECL_ID'
    SIMPLE_EXPR = 'SIMPLE_EXPR'

class Tokens(enum.Enum):
    IDENTIFIER = 'identifier'
    COLON = ':'
    INT = 'int'
    BOOL = 'bool'
    CHAR = 'char'
    EQUAL_TO = '='
    L_SQR_BRACKET = '['
    R_SQR_BRACKET = ']'
    L_CRL_BRCKT = '{'
    R_CRL_BRCKT = '}'
    L_PAREN = '('
    R_PAREN = ')'
    COMMA = ','
    SEMICOLON = ';'

def create_tokens():
    tokens = []
    with open('m1.io', 'rb') as f:
        _tokens = tokenize.tokenize(f.readline)
        for t in _tokens:
            if t.type in (1, 54):
                try:
                    new_t = Tokens(t.string)
                except ValueError:
                    new_t = Tokens.IDENTIFIER
                tokens.append(new_t)

    return tokens

