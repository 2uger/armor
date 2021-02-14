"""
    Steps to make parse table from grammar
    1. Create FIRST and FOLLOW of all non terminala
    2. Make LR0 automaton 
    3. Using FIRST and FOLLOW create parse table
"""
grammar = {'S': ['E', 'T'],
           'E': ['=']}

parse_table = {0: {TokenPlus : ('shift', 3),
                   TokenMinus: next_state2,},
               3: {TokenEqual: ('reduce', 'E', 1)}
              }

class Automaton():
    def __init__(self):
        pass

def follow(grammar, symbol):
    pass
def first(grammar, symbol):
    pass

def create_parse_table(automaton):
    for state in automaton.states:
        if terminal after dot:
            parse_table[state] = {terminal: ('shift', automaton.next_state(state, terminal)
        else if Nonterminal after dot:
            parse_table[state] = {Nonterminal: {'goto', automaton.next_state(state, Nonterminal)
        
        for item in state.items:
            if item == nothing after dot:
                parse_table[state] = {before_dot: {'reduce', automaton.prev_state(state, prev Non terminal)}}

class Parser():
    def __init__(self, tokens):
        self.tokens = tokens
        self.stack = []

        self.curr_state = 0

    def stack_push(self, symb):
        return self.stack.push(symb)

    def stack_pop(self):
        return self.stack.pop()

    def look_ahead(self):
        return self._tokens[0]

    def next_token(self):
        return self._tokens.pop()

    def parse(self):
        self.stack_push(self.curr_state)
        next_token = self.next_token()
        next_move = parse_table[self.curr_state][next_token]
        if parse_table[self.curr_state][next_token] == accept:
            return True
        else if next_move[0] == shift:
            self.stack_push(next_token)
            self.stack_push(next_move[1])
        else if next_move[0] == reduce:
            items_to_pop = reduce_rull[next_move[1]]
            self.stack_pop(items_to_pop)
            prev_state = self.stack_pop()
            next_state = parse_table[prev_state][NOTTERMINAL]
            self.stack_push(next_state)
