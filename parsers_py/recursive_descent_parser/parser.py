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
        assignment, index = parse_assignment(index+1)
    else:
        assignment = None

    index = match_token(index, TokenKind.SEMICOLON)

    return (var_type, var_name, assignment), index

def parse_assignment(index):
    l, index = parse_identifier(index)
    index = match_token(index, TokenKind.PLUS)
    r, index = parse_identifier(index)
    return (l, TokenKind.PLUS, r), index

# Func declaration parsing
def parse_func_decl(index):
    func_type, index = parse_type_spec(index)
    func_name, index = parse_identifier(index)
    index = match_token(index, TokenKind.L_PAREN)
    parms, index = parse_parms(index)
    index = match_token(index, TokenKind.R_PAREN)
    stmts, index = parse_compound_stmt(index)
    index = match_token(index, TokenKind.SEMICOLON)

    return [func_type, func_name, parms, stmts], index

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

def parse_statements(index):
    for f in (parse_compound_stmt, parse_if_statement, parse_return):
        try:
            return f(index)
        except Exception:
            print('Ruined on parse statements')
            continue
    return parse_expression_stmt(index)

def parse_compound_stmt(index):
    stmts = []
    index = match_token(index, TokenKind.L_CRL_BRCKT)

    while True:
        try:
            node, index = parse_statements(index)
            stmts.append(node)
        except Exception:
            break

    index = match_token(index, TokenKind.R_CRL_BRCKT)

    return stmts, index
    
def parse_return(index):
    index = match_token(index, TokenKind.RETURN)
    node, index = parse_expression(index)
    index = match_token(index, TokenKind.SEMICOLON)

    return [node], index

def parse_if_statement(index):
    index = match_token(index, TokenKind.IF)
    index = match_token(index, TokenKind.L_PAREN)
    if_cond, index = parse_expression(index)
    index = match_token(index, TokenKind.R_PAREN)
    if_stmt, index = parse_compound_stmt(index)

    if not token_is(index, TokenKind.ELSE):
        else_stmt = None
    else:
        else_stmt, index = parse_compound_stmt(index+1)

    return [if_cond, if_stmt, else_stmt], index
    
def parse_expression_stmt(index):
    if token_is(index, TokenKind.SEMICOLON):
        return [], index+1

    node, index = parse_expression(index)
    match_token(index, TokenKind.SEMICOLON)

    return node, index

def parse_expression(index):
    node, index = parse_assignment(index)
    return node, index

def parse_identifier(index):
    t = tokens[index]
    index = match_token(index, TokenKind.IDENTIFIER)
    return t, index
