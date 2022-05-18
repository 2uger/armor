import enum
import tokenize


class Token:
    def __init__(self, kind, content=''):
        self.kind = kind
        self.content = content if content else kind

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
    MUL = '*'


def create_tokens(file_name):
    tokens = []
    with open(file_name, 'rb') as f:
        _tokens = tokenize.tokenize(f.readline)

        if _tokens is None:
            return

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
