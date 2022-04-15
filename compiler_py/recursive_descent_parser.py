from tokens import *

tokens = []

def scan_token():
    return tokens.pop(0)

def putback_token(t):
    tokens.insert(0, t)

def expect_token(expect_t):
    t = scan_token()
    if t == expect_t:
        return True
    else:
        putback_token(t)
        return False

def parse_program():
    result = parse_decl()
    if result:
        print(f'MSG: successfully parse programm')
        return
    print(f'ERROR: error while parsing programm, check logs')


def parse_decl():
    return parse_func_decl()

# Var declaration parsing
def parse_var_decl():
    return parse_type_spec() and parse_var_decl_init() and scan_token() == Tokens.SEMICOLON

def parse_var_decl_init():
   t = scan_token()
   if expect_token(Tokens.EQUAL_TO):
       putback_token(t)
       return parse_var_decl_id() and parse_simple_expr()
   else:
       putback_token(t)
       return parse_var_decl_id()

def parse_var_decl_id():
    t = scan_token()
    if expect_token(Tokens.L_SQR_BRACKET):
        return t == Tokens.IDENTIFIER \
               and scan_token() == Tokens.L_SQR_BRACKET \
               and scan_token() == Tokens.NUMCONST \
               and scan_token() == Tokens.R_SQR_BRACKET
    else:
        return t == Tokens.IDENTIFIER

def parse_simple_expr():
    return True

# Func declaration parsing
def parse_func_decl():
    return parse_type_spec() \
           and expect_token(Tokens.IDENTIFIER) \
           and expect_token(Tokens.L_PAREN) \
           and parse_parms() \
           and expect_token(Tokens.R_PAREN) \
           and parse_stmt()

def parse_type_spec():
    t = scan_token()
    if t in (Tokens.INT, Tokens.CHAR):
        return True
    else:
        putback_token(t)
        print(f'ERROR: bad type spec {t}')
        return False

def parse_parms():
    t = scan_token()
    if t in (Tokens.INT, Tokens.CHAR):
        putback_token(t)
        return parse_parms_list()
    else:
        putback_token(t)
        return True

def parse_parms_list():
    return parse_parm_type() and parse_parm_list_prime()

def parse_parm_type():
    return parse_type_spec() and parse_identifier()

def parse_parm_list_prime():
    token = scan_token()
    if token == Tokens.COMMA:
        return parse_parm_type() and parse_parm_list_prime()
    else:
        putback_token(token)
        return True

def parse_stmt():
    return scan_token() == Tokens.L_CRL_BRCKT and scan_token() == Tokens.R_CRL_BRCKT

def parse_identifier():
    return scan_token() == Tokens.IDENTIFIER

def main():
    global tokens
    tokens = create_tokens()
    print(tokens)
    parse_program()

if __name__ == '__main__':
    main()
