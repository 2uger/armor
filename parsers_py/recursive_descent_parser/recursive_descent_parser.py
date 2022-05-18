from tokens import *

messages = []

def parse(tokens):
    result = parse_decl_list(tokens)
    messages.append(f'MSG: successfully parse programm' if result else f'ERROR: error while parsing programm, check logs')

def parse_decl_list(tokens):
    while 1:
        print(tokens)
        t = scan_token(tokens)
        if t == TokenKind.END:
            messages.append('Successfull parsing')
            return True 
        else:
            putback_token(tokens, t)
            res = parse_decl(tokens)
            if res == False:
                return False
    

def parse_decl(tokens):
    _type = scan_token(tokens)
    _ident = scan_token(tokens)

    if expect_token(tokens, TokenKind.EQUAL_TO) or expect_token(tokens, TokenKind.SEMICOLON):
        putback_token(tokens, _ident)
        putback_token(tokens, _type)
        print('Var')
        return parse_var_decl(tokens)
    else:
        putback_token(tokens, _ident)
        putback_token(tokens, _type)
        print('Func')
        print(tokens)
        return parse_func_decl(tokens)

    return True

# Var declaration parsing
def parse_var_decl(tokens):
    return parse_type_spec(tokens) \
           and parse_var_decl_init(tokens) \
           and scan_token(tokens) == TokenKind.SEMICOLON

def parse_var_decl_init(tokens):
   t = scan_token(tokens)
   if expect_token(tokens, TokenKind.EQUAL_TO):
       putback_token(tokens, t)
       return parse_var_decl_id(tokens) and scan_token(tokens) and parse_simple_expr(tokens)
   else:
       putback_token(tokens, t)
       return parse_var_decl_id(tokens)

def parse_var_decl_id(tokens):
    t = scan_token(tokens)
    return t == TokenKind.IDENTIFIER

def parse_simple_expr(tokens):
    t = scan_token(tokens)
    res = t == TokenKind.NUMCONST
    if res:
        return res
    else:
        messages.append(f'Bad simple expression: {t}')

# Func declaration parsing
def parse_func_decl(tokens):
    return parse_type_spec(tokens) \
           and scan_token(tokens) == TokenKind.IDENTIFIER \
           and scan_token(tokens) == TokenKind.L_PAREN \
           and parse_parms(tokens) \
           and scan_token(tokens) == TokenKind.R_PAREN \
           and scan_token(tokens) == TokenKind.SEMICOLON

def parse_type_spec(tokens):
    t = scan_token(tokens)
    if t in (TokenKind.INT,):
        return True
    else:
        putback_token(tokens, t)
        messages.append(f'ERROR: bad type spec {t}')
        return False

def parse_parms(tokens):
    if expect_tokens(tokens, [TokenKind.INT]):
        return parse_parms_list(tokens)
    else:
        return True

def parse_parms_list(tokens):
    return parse_parm_type(tokens) and parse_parm_list_prime(tokens)

def parse_parm_type(tokens):
    return parse_type_spec(tokens) and parse_identifier(tokens)

def parse_parm_list_prime(tokens):
    t = scan_token(tokens)
    if t == TokenKind.COMMA:
        return parse_parm_type(tokens) and parse_parm_list_prime(tokens)
    else:
        putback_token(tokens, t)
        return True


#def parse_stmt(tokens):
#    if expect_token(tokens, Tokens.IDENTIFIER):
#        return parse_expr_stmt(tokens)
#    elif expect_token(tokens, Tokens.L_CRL_BRCKT):
#        return parse_compound_stmt(tokens)
#    elif expect_token(tokens, Tokens.RETURN):
#        return parse_return_stmt(tokens)
#    else:
#        return True
#
#def parse_expr_stmt(tokens):
#    if expect_token(tokens, Tokens.IDENTIFIER):
#        return parse_expr(tokens) and scan_token(tokens) == Tokens.SEMICOLON
#    else:
#        return scan_token(tokens) == Tokens.SEMICOLON
#
#def parse_expr(tokens):
#    if scan_token(tokens) == Tokens.IDENTIFIER:
#        if scan_token(tokens) in (Tokens.INCREM, Tokens.DECREM):
#            return True
#        return parse_expr(tokens)
#    else:
#        return False
#
#def parse_compound_stmt(tokens):
#    return scan_token(tokens) == Tokens.L_CRL_BRCKT \
#           and parse_stmt_list(tokens) \
#           and scan_token(tokens) == Tokens.R_CRL_BRCKT
#
#def parse_stmt_list(tokens):
#    if expect_tokens(tokens, [Tokens.IDENTIFIER, Tokens.L_CRL_BRCKT, Tokens.RETURN]):
#        return parse_stmt(tokens) and parse_stmt_list(tokens)
#    elif t == Tokens.R_CRL_BRCKT:
#        return True
#    else:
#        return False
#
#def parse_return_stmt(tokens):
#    if scan_token(tokens) == Tokens.RETURN:
#        if expect_token(tokens, Tokens.IDENTIFIER):
#            return parse_expr(tokens) and scan_token(tokens) == Tokens.SEMICOLON
#        else:
#            return scan_token(tokens) == Tokens.SEMICOLON
#    else:
#        return False
#
def parse_identifier(tokens):
    t = scan_token(tokens)
    return t == TokenKind.IDENTIFIER
