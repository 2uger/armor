module Parser where
import Lexer 

data NonTerminal = S | E | Eprime | T | Tprime | F

parser :: [Token] -> Bool
parser [] = False
parser tokens = 
    let 
        (isOk, _) = parse S tokens
    in isOk

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

parse Eprime (token:remain)
    | token == TokenPlus = let 
                               (r1, remain')  = parse T remain
                               (r2, remain'') = parse Eprime remain'
                           in (r1 && r2, remain'')
    | otherwise = (True, token:remain)

parse Eprime x = (True, x)

parse T input = 
    let 
        (r1, remain)  = parse F input
        (r2, remain') = parse Tprime remain
    in (r1 && r2, remain')

parse Tprime (token:remain)
    | token == TokenMul = let 
                              (r1, remain')  = parse F remain
                              (r2, remain'') = parse Tprime remain'
                          in (r1 && r2, remain'')
    | otherwise = (True, token:remain)

parse Tprime x = (True, x)

parse F (token:remain)
    | token == TokenLParen = let 
                                 (r1, remain') = parse E remain
                             in case head remain' of TokenRParen -> (True, remain')
    | token == TokenTypeInt = (True, remain)
    | otherwise = (False, token:remain)

parse F x = (True, x)

