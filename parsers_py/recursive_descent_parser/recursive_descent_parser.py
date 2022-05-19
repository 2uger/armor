from tokens import *

messages = []

def parse(index):
    node, index = parse_decl_list(index)
    messages.append(f'MSG: successfully parse programm' if node else f'ERROR: error while parsing programm, check logs')
    
    return node

def parse_decl_list(index):
    decl_list = []
    while 1:
        if token_is(index, TokenKind.END):
            messages.append('Successfull parsing')
            return decl_list
        else:
            node, index = parse_decl(index)
            decl_list.append(node)
            if index == len(tokens) - 1:
                break

    return decl_list, index

def parse_decl(index):
    match_token(index, TokenKind.INT)
    match_token(index+1, TokenKind.IDENTIFIER)

    if token_in(index+2, (TokenKind.EQUAL_TO, TokenKind.SEMICOLON)):
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

    if token_is(index, TokenKind.EQUAL_TO):
        match_token(index, TokenKind.EQUAL_TO)
        assignment, index = parse_assignment(index+1)

    index = match_token(index, TokenKind.SEMICOLON)

    return (var_type, var_name, assignment), index

def parse_assignment(index):
    t = tokens[index]
    index = match_token(index, TokenKind.NUMCONST)
    return (t,), index

# Func declaration parsing
def parse_func_decl(index):
    func_type, index = parse_type_spec(index)
    func_name, index = parse_identifier(index)

    index = match_token(index, TokenKind.L_PAREN)
    parms, index = parse_parms(index)
    index = match_token(index, TokenKind.R_PAREN)

    index = match_token(index, TokenKind.SEMICOLON)

    return [func_type, func_name, parms], index

def parse_type_spec(index):
    t = tokens[index]
    index = match_token(index, TokenKind.INT)
    return t, index

def parse_parms(index):
    parms = []

    if token_is(index, TokenKind.R_PAREN):
        return parms, index
    
    while True:
        parm_type, index = parse_type_spec(index)
        parm_name, index = parse_identifier(index)

        parms.append([parm_type, parm_name])

        if token_is(index, TokenKind.COMMA):
            match_token(index, TokenKind.COMMA)
            index += 1
        else:
            break
    
    return parms, index

#def parse_parms_list(index):
#    return parse_parm_type(tokens) and parse_parm_list_prime(tokens)
#
#def parse_parm_type(index):
#    return parse_type_spec(tokens) and parse_identifier(tokens)
#
#def parse_parm_list_prime(index):
#    t = scan_token(tokens)
#    if t == TokenKind.COMMA:
#        return parse_parm_type(tokens) and parse_parm_list_prime(tokens)
#    else:
#        putback_token(tokens, t)
#        return True


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
    index = match_token(index, TokenKind.IDENTIFIER)
    return t, index
