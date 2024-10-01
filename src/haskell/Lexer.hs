module Lexer (lexemes, Lexeme(..)) where

import Control.Applicative
import Combinator (Consumer(..), valC, plusC, anyC, starC)
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

-- Lexer

data Lexeme
    = FillerL
    | SeperatorL String
    | KeywordL String
    | IntegerL String
    | FloatL String
    | StringL String
    | OperatorL String
    | IdentL String
    deriving (Show, Eq)

type Lexer a = Consumer [Char] a

charL :: Char -> Lexer Char
charL x = valC (== x)

notCharL :: Char -> Lexer Char
notCharL x = valC (/= x)

stringL :: String -> Lexer String
stringL = traverse charL

digitL :: Lexer Char
digitL = valC isDigit

integerL :: Lexer Lexeme
integerL = IntegerL <$> plusC digitL

whitespaceL :: Lexer Lexeme
whitespaceL = FillerL <$ plusC (valC isSpace)

operatorL :: Lexer Lexeme
operatorL = OperatorL <$> anyC stringL ["+", "*"]

alphaL :: Lexer Char
alphaL = valC isAlpha

alphaNumL :: Lexer Char
alphaNumL = valC isAlphaNum

underscoreL :: Lexer Char
underscoreL = charL '_'

identL :: Lexer Lexeme
identL = f <$> (alphaL <|> underscoreL)
           <*> starC (alphaNumL <|> underscoreL)
    where
        f start rest
            | (start:rest) `elem` ["print", "let"] = KeywordL (start:rest)
            | otherwise = IdentL $ start:rest


floatL :: Lexer Lexeme
floatL = f <$> plusC digitL <*> charL '.' <*> plusC digitL
    where f x dot y = FloatL $ x ++ [dot] ++ y

stringValL :: Lexer Lexeme
stringValL = f <$> charL '"' <*> starC (notCharL '"') <*> charL '"'
    where f start contents end = StringL $ [start] ++ contents ++ [end]

seperatorL :: Lexer Lexeme
seperatorL = SeperatorL <$> anyC stringL ["(", ")", "<-", ";"]

lexeme :: Lexer Lexeme
lexeme =
    whitespaceL
    <|> seperatorL
    <|> floatL
    <|> integerL
    <|> stringValL
    <|> operatorL
    <|> identL

lexemes :: String -> Maybe [Lexeme]
lexemes input =
    case run (starC lexeme) input of
        Just (xs, []) -> Just $ filter (/= FillerL) xs
        _             -> Nothing

