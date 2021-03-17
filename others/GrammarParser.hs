module GrammarParser where

import ParserTypes
   
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative

parseGrammar :: String -> Grammar
parseGrammar grammarText = concat 
                           $ map concat 
                           $ map (Parsec.parse parseProduction "(source)") 
                           $ filterGrammar grammarLine
  where
    grammarLine = lines grammarText
    -- To remove empty strings and comments
    filterGrammar :: [String] -> [String]
    filterGrammar = filter filterComments . filter filterEmpty
      where 
        filterEmpty = \x -> x /= ""
        filterComments = \x -> head x /= '#' 
    parseProduction :: Parsec.Parsec String () [Production]
    parseProduction = do
        lhs <- Parsec.many Parsec.letter
        separator
        rhs <- parseRhs
        return $ map (createProduction lhs) rhs
      where
        separator = Parsec.spaces >> Parsec.string "::" >> Parsec.spaces
        createProduction lhs x = Production (NTerm lhs) x

        parseRhs :: Parsec.Parsec String () [[Symbol]] 
        parseRhs =  Parsec.sepBy parseRhsPiece rhsSep
          where 
            rhsSep = Parsec.spaces >> Parsec.char '|' >> Parsec.spaces
                -- Rhs piece is part of production between |
                -- Terms and NonTerms could be here
                -- Terms stays in quote 'term'
            parseRhsPiece :: Parsec.Parsec String () [Symbol]
            parseRhsPiece = Parsec.many $ do
                symbol <- (parseNTerm <|> parseTerm) 
                Parsec.spaces
                return symbol
              where
                parseNTerm :: Parsec.Parsec String () Symbol
                parseNTerm = do
                    nterm <- Parsec.many1 Parsec.letter <?> "Error parsing non terminal"
                    return (Right $ NTerm nterm)
                parseTerm :: Parsec.Parsec String () Symbol
                parseTerm = do
                    Parsec.string "'" <?> "First quote is empty"
                    term <- parseWord <|> parseSign <?> "Strange sign"
                    Parsec.string "'"
                    case term of
                        "E" -> return (Left Epsilon)
                        _ -> return (Left $ Term term)
                  where
                    parseSign = Parsec.many1 $ Parsec.oneOf ":%&*{},;/[]()"
                    parseWord = Parsec.many1 Parsec.letter
