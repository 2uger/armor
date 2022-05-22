import enum
import tokenize

tokens = []

class Token:
    def __init__(self, kind, content=''):
        self.kind = kind
        self.content = content

    def __repr__(self):
        return f'Token: {self.kind}'

    def __eq__(self, t):
        return self.kind == t

class TokenKind(enum.Enum):
    END = '$'
    EMPTY = 'empty'
    IDENTIFIER = 'identifier'
    NUMCONST = 'numconst'
    KEYWORD = 'keyword'
    SYMBOL = 'symbol'

    INT = 'int'
    BOOL = 'bool'
    RETURN = 'return'
    IF = 'if'
    ELSE = 'else'

    COLON = ':'
    SEMICOLON = ';'

    L_SQR_BRCKT = '['
    R_SQR_BRCKT = ']'
    L_CRL_BRCKT = '{'
    R_CRL_BRCKT = '}'
    L_PAREN = '('
    R_PAREN = ')'

    COMMA = ','
    EQUAL_TO = '='
    PLUS = '+'
    MINUS = '-'
    MUL = '*'
    DIV = '/'

    BT = '>'
    LT = '<'
    EQ = '=='

def create_tokens(file_name):
    global tokens
    with open(file_name, 'rb') as f:
        _tokens = tokenize.tokenize(f.readline)

        if _tokens is None:
            return False

        for t in _tokens:
            if t.type in (1, 2, 54):
                try:
                    if t.type == 2:
                        n_t = Token(TokenKind.NUMCONST, int(t.string))
                    else:
                        t_kind = TokenKind(t.string)
                        n_t = Token(t_kind)
                except ValueError:
                    n_t = Token(TokenKind.IDENTIFIER, t.string)
                tokens.append(n_t)
        tokens.append(Token(TokenKind.END))

    return True

def token_is(index, kind):
    return len(tokens) > index and tokens[index].kind == kind

def token_in(index, kinds):
    return len(tokens) > index and tokens[index].kind in kinds

def match_token(index, kind):
    if token_is(index, kind):
        return index + 1
    else:
        raise Exception(f'Error matching token: {kind}, current token: {tokens[index]}')
    