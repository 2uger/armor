module GrammarParser where

import ParserTypes
   
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative


parseGrammar :: String -> [Rule]
parseGrammar grammarText = map grammarFromRule $ map (Parsec.parse parseRule "(source)") grammarLine
  where
    grammarLine = lines grammarText

    grammarFromRule :: Either Parsec.ParseError Rule -> Rule
    grammarFromRule parseRule = 
        case parseRule of
          Right rule -> rule

    parseRule :: Parsec.Parsec String () Rule
    parseRule = do
        prod <- Parsec.many Parsec.letter
        separator
        rhs <- parseRhs
        return $ Rule (NTerm prod) rhs
      where
        separator = Parsec.spaces >> Parsec.string "::" >> Parsec.spaces

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
                    nterm <- Parsec.many1 Parsec.letter
                    return (Right $ NTerm nterm)

                parseTerm :: Parsec.Parsec String () Symbol
                parseTerm = do
                    Parsec.string "'"
                    term <- parseWord <|> parseSign
                    Parsec.string "'"
                    case term of
                        "E" -> return (Left Epsilon)
                        _ -> return (Left $ Term term)
                  where
                    parseSign = Parsec.many1 $ Parsec.oneOf "%&*{},;/[]()"
                    parseWord = Parsec.many1 Parsec.letter
