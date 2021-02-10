module Parse where
import Lexer 

data NonTerminal = S | E | Eprime | T | Tprime | F

parse :: NonTerminal -> [Token] -> (Bool, [Token])
parse S input = 
    let 
        (r1, remain) = parse E input 
    in (r1, remain) 

parse E input =
    let
        (r1, remain)  = parse T input
        (r2, remain') = parse Eprime remain
    in (r1 && r2, remain')

parse Eprime input@(token:remain)
    | token == TokenPlus = let 
                               (r1, remain')  = parse T remain
                               (r2, remain'') = parse Eprime remain'
                           in (r1 && r2, remain'')
    | otherwise = (True, input)

parse T input = 
    let 
        (r1, remain)  = parse F input
        (r2, remain') = parse Tprime remain
    in (r1 && r2, remain')

parse Tprime input@(token:remain)
    | token == TokenMul = let 
                              (r1, remain')  = parse F remain
                              (r2, remain'') = parse Tprime remain'
                          in (r1 && r2, remain'')
    | otherwise = (True, input)

parse F input@(token:nextToken:remain)
    | token == TokenLParen = let 
                                       (r1, remain') = parse E remain
                                   in case nextToken of TokenRParen -> (True, remain')
    | token == TokenInt = (True, nextToken:remain)
    | otherwise = (False, input)
