from tokens import *
import ast as node

messages = []

def parse(index):
    items, index = parse_decl_list(index)
    messages.append(f'MSG: successfully parse programm' if node else f'ERROR: error while parsing programm, check logs')
    
    return node.Programm(items)

def parse_decl_list(index):
    items = []
    while 1:
        if token_is(index, TokenKind.END):
            messages.append('Successfull parsing')
            return items
        else:
            node, index = parse_decl(index)
            items.append(node)
            if index == len(tokens) - 1:
                break

    return items, index

def parse_decl(index):
    match_token(index, TokenKind.INT)
    match_token(index+1, TokenKind.IDENTIFIER)

    if token_in(index+2, (TokenKind.EQUAL_TO, TokenKind.SEMICOLON)):
        print('Var')
        return parse_var_decl(index)
    else:
        print('Func')
        return parse_func_definition(index)

def parse_var_decl(index):
    spec, index = parse_type_spec(index)
    decl, index = parse_identifier(index)

    if token_is(index, TokenKind.EQUAL_TO):
        init, index = parse_assignment(index+1)
    else:
        init = None

    index = match_token(index, TokenKind.SEMICOLON)

    return node.Declaration(node.DeclarationRoot(spec, decl, init)), index

# Function definition parser
def parse_func_definition(index):
    spec, index = parse_type_spec(index)
    decl, index = parse_func_declarator(index)

    body, index = parse_compound_stmt(index)
    index = match_token(index, TokenKind.SEMICOLON)

    root = node.DeclarationRoot(spec, decl)

    return node.Declaration(root, body), index

def parse_func_declarator(index):
    identifier, index = parse_identifier(index)

    index = match_token(index, TokenKind.L_PAREN)
    parms, index = parse_parms(index)
    index = match_token(index, TokenKind.R_PAREN)

    return node.Function(identifier, parms), index

def parse_type_spec(index):
    t = tokens[index]
    index = match_token(index, TokenKind.INT)
    return t, index

def parse_parms(index):
    items = []
    if token_is(index, TokenKind.R_PAREN):
        return items, index
    
    while True:
        spec, index = parse_type_spec(index)
        name, index = parse_identifier(index)
        items.append(node.DeclarationRoot(spec, name))

        if token_is(index, TokenKind.COMMA):
            match_token(index, TokenKind.COMMA)
            index += 1
        else:
            break
    
    return items, index

def parse_statements(index):
    for f in (parse_compound_stmt, parse_if_statement, parse_return):
        try:
            return f(index)
        except Exception:
            continue
    return parse_expression_stmt(index)

def parse_compound_stmt(index):
    items = []
    index = match_token(index, TokenKind.L_CRL_BRCKT)

    while True:
        try:
            stmt_node, index = parse_statements(index)
            items.append(stmt_node)
        except Exception:
            break

    index = match_token(index, TokenKind.R_CRL_BRCKT)

    return node.Compound(items), index
    
def parse_return(index):
    index = match_token(index, TokenKind.RETURN)
    ret_value, index = parse_expression(index)
    index = match_token(index, TokenKind.SEMICOLON)

    return node.Return(ret_value), index

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

    return node.IfStatement(if_cond, if_stmt, else_stmt), index
    
def parse_expression_stmt(index):
    if token_is(index, TokenKind.SEMICOLON):
        return node.EmptyNode(), index+1

    expr_node, index = parse_expression(index)
    match_token(index, TokenKind.SEMICOLON)

    print('Hello')
    print(expr_node)
    return expr_node, index

def parse_identifier(index):
    t = tokens[index]
    index = match_token(index, TokenKind.IDENTIFIER)
    return node.Identifier(t.content), index

####### Parsers for expressions
def parse_assignment(index):
    left, index = parse_conditional(index)

    op = tokens[index]
    
    node_types = {
        TokenKind.PLUS: node.Plus,
        TokenKind.MINUS: node.Minus,
        TokenKind.MUL: node.Mul,
        TokenKind.EQUAL_TO: node.Equals,
        TokenKind.BT: node.BiggerThan,
        TokenKind.LT: node.LessThan,
        TokenKind.EQ: node.Equal
    }

    if op.kind in node_types:
        right, index = parse_assignment(index+1)
        return node_types[op.kind](left, right, op), index
    else:
        return left, index

def parse_expression(index):
    node, index = parse_assignment(index)
    return node, index

def parse_conditional(index):
    return parse_primary(index)

def parse_primary(index):
    if token_is(index, TokenKind.NUMCONST):
        t = tokens[index]
        return node.Number(t.content), index + 1 
    elif token_is(index, TokenKind.IDENTIFIER):
        t = tokens[index]
        return node.Identifier(t.content), index + 1
