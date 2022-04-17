from tokens import *


def parse_program(tokens):
    result = parse_decl_list(tokens)
    msg = f'MSG: successfully parse programm' if result else f'ERROR: error while parsing programm, check logs'
    print(msg)

def parse_decl_list(tokens):
    decl_res = parse_decl(tokens)
    if decl_res:
        t = scan_token(tokens)
        if t in (Tokens.VAR, Tokens.FUNC):
            putback_token(tokens, t)
            return parse_decl_list(tokens)
        else:
            return True
    return False

def parse_decl(tokens):
    t = scan_token(tokens)
    if t == Tokens.VAR:
        return parse_var_decl(tokens)
    if t == Tokens.FUNC:
        return parse_func_decl(tokens)
    return False

# Var declaration parsing
def parse_var_decl(tokens):
    return parse_type_spec(tokens) and parse_var_decl_init(tokens) and scan_token(tokens) == Tokens.SEMICOLON

def parse_var_decl_init(tokens):
   t = scan_token(tokens)
   if expect_token(tokens, Tokens.EQUAL_TO):
       putback_token(tokens, t)
       return parse_var_decl_id(tokens) and parse_simple_expr(tokens)
   else:
       putback_token(tokens, t)
       return parse_var_decl_id(tokens)

def parse_var_decl_id(tokens):
    t = scan_token(tokens)
    if expect_token(tokens, Tokens.L_SQR_BRACKET):
        return t == Tokens.IDENTIFIER \
               and scan_token(tokens) == Tokens.L_SQR_BRACKET \
               and scan_token(tokens) == Tokens.NUMCONST \
               and scan_token(tokens) == Tokens.R_SQR_BRACKET
    else:
        return t == Tokens.IDENTIFIER

def parse_simple_expr(tokens):
    return True

# Func declaration parsing
def parse_func_decl(tokens):
    return parse_type_spec(tokens) \
           and scan_token(tokens) == Tokens.IDENTIFIER \
           and scan_token(tokens) == Tokens.L_PAREN \
           and parse_parms(tokens) \
           and scan_token(tokens) == Tokens.R_PAREN \
           and parse_stmt(tokens) \

def parse_type_spec(tokens):
    t = scan_token(tokens)
    if t in (Tokens.INT, Tokens.CHAR):
        return True
    else:
        putback_token(tokens, t)
        print(f'ERROR: bad type spec {t}')
        return False

def parse_parms(tokens):
    t = scan_token(tokens)
    if t in (Tokens.INT, Tokens.CHAR):
        putback_token(tokens, t)
        return parse_parms_list(tokens)
    else:
        putback_token(tokens, t)
        return True

def parse_parms_list(tokens):
    return parse_parm_type(tokens) and parse_parm_list_prime(tokens)

def parse_parm_type(tokens):
    return parse_type_spec(tokens) and parse_identifier(tokens)

def parse_parm_list_prime(tokens):
    t = scan_token(tokens)
    if t == Tokens.COMMA:
        return parse_parm_type(tokens) and parse_parm_list_prime(tokens)
    else:
        putback_token(tokens, t)
        return True

def parse_stmt(tokens):
    if expect_token(tokens, Tokens.IDENTIFIER):
        return parse_expr_stmt(tokens)
    elif expect_token(tokens, Tokens.L_CRL_BRCKT):
        return parse_compound_stmt(tokens)
    elif expect_token(tokens, Tokens.RETURN):
        return parse_return_stmt(tokens)
    else:
        return True

def parse_expr_stmt(tokens):
    if expect_token(tokens, Tokens.IDENTIFIER):
        return parse_expr(tokens) and scan_token(tokens) == Tokens.SEMICOLON
    else:
        return scan_token(tokens) == Tokens.SEMICOLON

def parse_expr(tokens):
    if scan_token(tokens) == Tokens.IDENTIFIER:
        if scan_token(tokens) in (Tokens.INCREM, Tokens.DECREM):
            return True
        return parse_expr(tokens)
    else:
        return False

def parse_compound_stmt(tokens):
    return scan_token(tokens) == Tokens.L_CRL_BRCKT \
           and parse_stmt_list(tokens) \
           and scan_token(tokens) == Tokens.R_CRL_BRCKT

def parse_stmt_list(tokens):
    t = scan_token(tokens)
    if t and t in (Tokens.IDENTIFIER, Tokens.L_CRL_BRCKT, Tokens.RETURN):
        putback_token(tokens, t)
        return parse_stmt(tokens) and parse_stmt_list(tokens)
    elif t == Tokens.R_CRL_BRCKT:
        putback_token(tokens, t)
        return True
    else:
        return False

def parse_return_stmt(tokens):
    if scan_token(tokens) == Tokens.RETURN:
        if expect_token(tokens, Tokens.IDENTIFIER):
            return parse_expr(tokens) and scan_token(tokens) == Tokens.SEMICOLON
        else:
            return scan_token(tokens) == Tokens.SEMICOLON
    else:
        return False


def parse_identifier(tokens):
    t = scan_token(tokens)
    return t == Tokens.IDENTIFIER

