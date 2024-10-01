module Evaluator (tokens, Token(..)) where

import Lexer (Lexeme(..))

-- Evaluator

data Token
    = IntegerT Int
    | OpenParenT
    | CloseParenT
    | AssignmentT
    | EndStatementT
    | KeywordPrintT
    | FloatT Float
    | StringT String
    | OperatorT String
    | IdentT String
    | ArrayT [Token]  -- Temporary, only constructed for interpreter
    deriving (Show, Eq)

safeRead :: (Read a) => String -> Maybe a
safeRead s =
    case reads s of
        [(x, "")] -> Just x
        _         -> Nothing

evaluate :: Lexeme -> Maybe Token
evaluate (IntegerL s)       = fmap IntegerT (safeRead s)
evaluate (FloatL s)         = fmap FloatT (safeRead s)
evaluate (StringL s)        = Just $ StringT $ init $ tail s
evaluate (OperatorL s)      = Just $ OperatorT s
evaluate (IdentL s)         = Just $ IdentT s
evaluate (SeperatorL "(")   = Just OpenParenT
evaluate (SeperatorL ")")   = Just CloseParenT
evaluate (SeperatorL "<-")  = Just AssignmentT
evaluate (SeperatorL ";")   = Just EndStatementT
evaluate (KeywordL "print") = Just KeywordPrintT
evaluate _                  = Nothing

tokens :: [Lexeme] -> Maybe [Token]
tokens = traverse evaluate

