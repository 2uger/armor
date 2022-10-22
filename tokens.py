import enum

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

    # Keywords
    VOID = 'void'
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
    BE = '>='
    LT = '<'
    LE = '<='
    NE = '!='
    EQ = '=='

def create_tokens(file_name):
    with open(file_name, 'r') as f:
        line = f.readline()
        while line != '':
            lexify_string(line)
            line = f.readline()
    tokens.append(Token(TokenKind.END))

def lexify_string(line):
    if line[len(line) - 1] != '\n':
        line = line + '\n'
    curr_p, start_p = 0, 0
    s = line[curr_p]
    while (s != '\n'):
        token = None
        if (s == ','): token = Token(TokenKind.COMMA)
        elif (s == ':'): token = Token(TokenKind.COLON)
        elif (s == ';'): token = Token(TokenKind.SEMICOLON)
        elif (s == '['): token = Token(TokenKind.L_SQR_BRCKT)
        elif (s == ']'): token = Token(TokenKind.R_SQR_BRCKT)
        elif (s == '{'): token = Token(TokenKind.L_CRL_BRCKT)
        elif (s == '}'): token = Token(TokenKind.R_CRL_BRCKT)
        elif (s == '('): token = Token(TokenKind.L_PAREN)
        elif (s == ')'): token = Token(TokenKind.R_PAREN)
        elif (s == '+'): token = Token(TokenKind.PLUS)
        elif (s == '-'): token = Token(TokenKind.MINUS)
        elif (s == '*'): token = Token(TokenKind.MUL)
        elif (s == '/'): token = Token(TokenKind.DIV)
        elif (s == '!'): 
            if line[curr_p + 1] == '=':
                curr_p += 1
                token = Token(TokenKind.NE)
            else:
                raise Exception(f'unknown symbol for not equal operator: {line[curr_p + 1]}')
        elif (s == '>'):
            if line[curr_p + 1] == '=':
                curr_p += 1
                token = Token(TokenKind.BE)
            else:
                token = Token(TokenKind.BT)
        elif (s == '<'):
            if line[curr_p + 1] == '=':
                curr_p += 1
                token = Token(TokenKind.LE)
            else:
                token = Token(TokenKind.LT)
        elif (s == '='):
            if line[curr_p + 1] == '=':
                curr_p += 1
                token = Token(TokenKind.EQ)
            else:
                token = Token(TokenKind.EQUAL_TO)
        # Identifier
        elif ((s >= 'a' and s <= 'z') or (s >= 'A' and s <= 'Z') or (s == '_')):
            curr_p += 1
            s = line[curr_p]
            while ((s >= 'a' and s <= 'z') or (s >= 'A' and s <= 'Z') or (s == '_') or (s >= '1' and s <= '9') and curr_p < len(line) - 1):
                curr_p += 1
                s = line[curr_p]
            curr_p -= 1
            lex = line[start_p: curr_p + 1]
            try:
                t_kind = TokenKind(lex)
                token = Token(t_kind)
            except ValueError:
                token = Token(TokenKind.IDENTIFIER, lex)
        # Numbers
        elif (s >= '1' and s <= '9'):
            while (s >= '1' and s <= '9' and curr_p < len(line) - 1):
                curr_p += 1
                s = line[curr_p]
            curr_p -= 1
            token = Token(TokenKind.NUMCONST, int(line[start_p: curr_p + 1]))

        curr_p += 1
        start_p = curr_p
        s = line[curr_p]

        if token:
            tokens.append(token)

def token_is(index, kind):
    return len(tokens) > index and tokens[index].kind == kind

def token_in(index, kinds):
    return len(tokens) > index and tokens[index].kind in kinds

def match_token(index, kind):
    if token_is(index, kind):
        return index + 1
    else:
        raise Exception(f'Error matching token: {kind}, current token: {tokens[index]}')
    