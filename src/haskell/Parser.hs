module Parser (parse, AST(..), BaseAST(..)) where

import Control.Applicative
import Combinator (Consumer(..), valC, plusC)
import Evaluator (Token(..))

-- Parser

data AST a
    = EmptyAST
    | Literal Token
    | Ident Token
    | Array [a]
    | Print a
    | Expr a Token a
    deriving (Show)

newtype BaseAST = BaseAST (AST BaseAST)
    deriving (Show)

type Parser a = Consumer [Token] a

isLiteral :: Token -> Bool
isLiteral (IntegerT _) = True
isLiteral (FloatT _)   = True
isLiteral (StringT _)  = True
isLiteral _            = False

isOperator :: Token -> Bool
isOperator (OperatorT _) = True
isOperator _             = False

isIdent :: Token -> Bool
isIdent (IdentT _) = True
isIdent _             = False

literalP :: Parser BaseAST
literalP = BaseAST . Literal <$> valC isLiteral

operatorP :: Parser Token
operatorP = valC isOperator

identP :: Parser BaseAST
identP = BaseAST . Ident <$> valC isIdent

tokenP :: Token -> Parser Token
tokenP tok = valC (== tok)

exprP :: Parser BaseAST
exprP = BaseAST <$> (Expr <$> (arrayP <|> parenExprP <|> identP <|> literalP)
                          <*> valC isOperator
                          <*> baseASTP)

printP :: Parser BaseAST
printP =  BaseAST . Print <$> (tokenP KeywordPrintT *> exprP)

elementP :: Parser BaseAST
elementP = parenExprP <|> literalP

arrayP :: Parser BaseAST
arrayP = f <$> elementP <*> plusC elementP
    where f el rest = BaseAST $ Array (el:rest)

parenExprP :: Parser BaseAST
parenExprP = tokenP OpenParenT *> baseASTP <* tokenP CloseParenT

assignmentP :: Parser BaseAST
assignmentP = BaseAST <$> (Expr <$> identP <*> tokenP AssignmentT <*> baseASTP)

baseASTP :: Parser BaseAST
baseASTP =
    assignmentP
    <|> printP
    <|> exprP
    <|> arrayP
    <|> parenExprP
    <|> identP
    <|> literalP

parse :: [Token] -> Maybe BaseAST
parse input =
    case run baseASTP input of
        Just (x, []) -> Just x
        _            -> Nothing

