import enum
import tokenize

class Tokens(enum.Enum):
    EMPTY = 'empty'
    IDENTIFIER = 'identifier'
    VAR = 'var'
    FUNC = 'func'
    COLON = ':'
    INT = 'int'
    BOOL = 'bool'
    CHAR = 'char'
    RETURN = 'return'
    EQUAL_TO = '='
    L_SQR_BRACKET = '['
    R_SQR_BRACKET = ']'
    L_CRL_BRCKT = '{'
    R_CRL_BRCKT = '}'
    L_PAREN = '('
    R_PAREN = ')'
    COMMA = ','
    SEMICOLON = ';'
    INCREM = '++'
    DECREM = '--'
    NUMCONST = 'numconst'

def create_tokens():
    tokens = []
    with open('m1.io', 'rb') as f:
        _tokens = tokenize.tokenize(f.readline)

        # to find increment and decrement signs
        prev_plus = False
        prev_minus = False

        for t in _tokens:
            if t.type in (1, 54):
                try:
                    if t.string == '+' and prev_plus:
                        new_t = Tokens.INCREM
                        prev_plus = False
                    elif t.string == '+' and not prev_plus:
                        prev_plus = True
                        continue
                    elif t.string == '-' and prev_minus:
                        new_t = Tokens.DECREM
                        prev_minus = False
                    elif t.string == '-' and not prev_minus:
                        prev_minus= True
                        continue
                    else:
                        new_t = Tokens(t.string)
                except ValueError:
                    new_t = Tokens.IDENTIFIER
                tokens.append(new_t)

    return tokens

def scan_token(tokens):
    if tokens:
        return tokens.pop(0)
    return None

def putback_token(tokens, t):
    tokens.insert(0, t)
    return tokens

def expect_token(tokens, expect_t):
    t = scan_token(tokens)
    if t and t == expect_t:
        putback_token(tokens, t)
        return True
    else:
        putback_token(tokens, t)
        return False

def expect_tokens(tokens, expect_ts):
    t = scan_token(tokens)
    if t and t in expect_ts:
        putback_token(tokens, t)
        return True
    else:
        putback_token(tokens, t)
        return False
