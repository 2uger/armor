from tokens import *

messages = []

def parse(index):
    result = parse_decl_list(index)
    messages.append(f'MSG: successfully parse programm' if result else f'ERROR: error while parsing programm, check logs')

def parse_decl_list(index):
    decl_list = []
    while 1:
        print(tokens)
        if token_is(index, TokenKind.END)
            messages.append('Successfull parsing')
            return decl_list
        else:
            node = parse_decl(index)
            decl_list.append(node)
    

def parse_decl(index):
    token_is(index, TokenKind.INT)
    token_is(index+1, TokenKind.IDENITIFIER)

    if token_in(index+2, (TokenKind.EQUAL_TO, TokenKind.SEMICOLON))
        print('Var')
        return parse_var_decl(index)
    else:
        print('Func')
        return parse_func_decl(index)

# Var declaration parsing
def parse_var_decl(index):
    var_type, index = parse_type_spec(index)
    var_name, index = parse_identifier(index)
    assignment = None

    if token_is(TokenKind.EQUAL_TO):
        assignment, index = parse_assignment(index)

    return node.VarDecl(var_type, var_id, assignment), index

def parse_assignment(index):
    index = match_token(TokenKind,NUMCONST)
    return node.Expression(), index

# Func declaration parsing
def parse_func_decl(index):
    return parse_type_spec(tokens) \
           and scan_token(tokens) == TokenKind.IDENTIFIER \
           and scan_token(tokens) == TokenKind.L_PAREN \
           and parse_parms(tokens) \
           and scan_token(tokens) == TokenKind.R_PAREN \
           and scan_token(tokens) == TokenKind.SEMICOLON

def parse_type_spec(index):
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

def parse_parms_list(index):
    return parse_parm_type(tokens) and parse_parm_list_prime(tokens)

def parse_parm_type(index):
    return parse_type_spec(tokens) and parse_identifier(tokens)

def parse_parm_list_prime(index):
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
def parse_identifier(index):
    t = tokens[index]
    index = match_token(TokenKind.IDENTIFIER)
    return t.content, index
