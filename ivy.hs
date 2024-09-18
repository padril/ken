import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import System.IO (hFlush, stdout)


-- Consumer ADT

newtype Consumer i o = Consumer { run :: i -> Maybe (o, i) }

instance Functor (Consumer i) where
    fmap f (Consumer l) =
        Consumer $ \input -> do
            (result, rest) <- l input
            Just (f result, rest)

instance Applicative (Consumer i) where
    pure x = Consumer $ \input -> Just (x, input)
    Consumer l1 <*> Consumer l2 =
        Consumer $ \input -> do
            (f, input') <- l1 input
            (x, input'') <- l2 input'
            Just (f x, input'')

instance Alternative (Consumer i) where
    empty = Consumer $ const Nothing
    Consumer l1 <|> Consumer l2 = Consumer $ \input -> l1 input <|> l2 input

anyC :: (a -> Consumer i o) -> [a] -> Consumer i o
anyC f xs = foldl1 (<|>) (map f xs)

starC :: Consumer i o -> Consumer i [o]
starC lexer@(Consumer l) = Consumer $ \input ->
    case l input of
        Just (x, input') -> run ((x:) <$> starC lexer) input'
        Nothing -> Just ([], input)

plusC :: Consumer i o -> Consumer i [o]
plusC lexer@(Consumer l) = Consumer $ \input -> do
   (first, input') <- l input
   (final, input'') <- run (starC lexer) input
   Just (final, input'')

valC :: (i -> Bool) -> Consumer [i] i
valC filter = Consumer f
    where
        f (y:ys)
            | filter y = Just (y, ys)
            | otherwise = Nothing
        f [] = Nothing


-- Lexer

data Lexeme
    = FillerL
    | SeperatorL String
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
    where f start rest = IdentL $ start:rest

floatL :: Lexer Lexeme
floatL = f <$> plusC digitL <*> charL '.' <*> plusC digitL
    where f x dot y = FloatL $ x ++ [dot] ++ y

stringValL :: Lexer Lexeme
stringValL = f <$> charL '"' <*> starC (notCharL '"') <*> charL '"'
    where f start contents end = StringL $ [start] ++ contents ++ [end]

seperatorL :: Lexer Lexeme
seperatorL = SeperatorL <$> anyC stringL ["(", ")", "<-"]

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


-- Evaluator

data Token
    = IntegerT Int
    | OpenParenT
    | CloseParenT
    | AssignmentT
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
evaluate (IntegerL s)      = fmap IntegerT (safeRead s)
evaluate (FloatL s)        = fmap FloatT (safeRead s)
evaluate (StringL s)       = Just $ StringT $ init $ tail s
evaluate (OperatorL s)     = Just $ OperatorT s
evaluate (IdentL s)        = Just $ IdentT s
evaluate (SeperatorL "(")  = Just OpenParenT
evaluate (SeperatorL ")")  = Just CloseParenT
evaluate (SeperatorL "<-") = Just AssignmentT
evaluate _                 = Nothing

tokens :: [Lexeme] -> Maybe [Token]
tokens = traverse evaluate


-- Parser

data AST a
    = Literal Token
    | Ident Token
    | Array [a]
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

elementP :: Parser BaseAST
elementP = parenExprP <|> literalP

arrayP :: Parser BaseAST
arrayP = f <$> elementP <*> plusC elementP
    where f el rest = BaseAST $ Array (el:rest)

parenExprP :: Parser BaseAST
parenExprP = tokenP OpenParenT *> baseASTP <* tokenP CloseParenT

assignmentP :: Parser BaseAST
assignmentP = BaseAST <$> (Expr <$> identP <*> tokenP AssignmentT <*> baseASTP)

baseASTP =
    assignmentP
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


-- Type Checker

type SimpleEnvMap = Map.Map String Type
type SimpleEnv = [SimpleEnvMap]

mergeEnv :: (Ord a) => [Map.Map a b] -> [Map.Map a b] -> [Map.Map a b]
mergeEnv (x:xs) (y:_) = (x `Map.union` y):xs

getEnv :: (Ord a) => [Map.Map a b] -> a -> Maybe b
getEnv env k = foldl (<|>) Nothing $ map (Map.lookup k) env

data Type
    = IntegerType
    | FloatType
    | StringType
    | OperatorType Type Type Type
    | ArrayType Type
    deriving (Eq, Show, Ord)

data TypedAST = TypedAST Type (AST TypedAST)
    deriving (Show)

operatorMap :: Map.Map String (Map.Map (Type, Type) Type)
operatorMap =
    Map.fromList
        [("+", Map.fromList [((IntegerType, IntegerType), IntegerType)
                            ,((FloatType, FloatType), FloatType)
                            ])
        ,("*", Map.fromList [((IntegerType, IntegerType), IntegerType)
                            ,((FloatType, FloatType), FloatType)
                            ])
        ]

deArray :: Type -> Type
deArray (ArrayType t) = t
deArray t             = t

check :: BaseAST -> SimpleEnv -> Maybe (TypedAST, SimpleEnv)
check (BaseAST (Literal tok@(IntegerT _))) env =
    Just (TypedAST IntegerType (Literal tok), env)
check (BaseAST (Literal tok@(FloatT _))) env =
    Just (TypedAST FloatType (Literal tok), env)
check (BaseAST (Literal tok@(StringT _))) env =
    Just (TypedAST StringType (Literal tok), env)
check (BaseAST (Ident tok@(IdentT ident))) env = do
    t <- getEnv env ident
    Just (TypedAST t (Ident tok), env)
check (BaseAST (Expr (BaseAST (Ident (IdentT x))) AssignmentT y)) env = do
    (y'@(TypedAST yt _), yEnvH:yEnvT) <- check y env
    let env' = Map.insert x yt yEnvH:yEnvT
    Just (TypedAST yt (Expr (TypedAST yt (Ident (IdentT x))) AssignmentT y'), env')
check (BaseAST (Expr x f@(OperatorT repr) y)) env = do
    (x'@(TypedAST xt _), xEnv) <- check x env
    (y'@(TypedAST yt _), yEnv) <- check y env
    let env' = mergeEnv xEnv yEnv
    poly <- Map.lookup repr operatorMap
    ft <- Map.lookup (deArray xt, deArray yt) poly
    Just (TypedAST ft (Expr x' f y'), env')
check (BaseAST (Array xs)) env = do
    (xs', envs) <- mapAndUnzipM (`check` env) xs
    let xts = map extractType xs'
    let env' = foldl1 mergeEnv envs
    xst <- ArrayType <$> foldEq xts
    Just (TypedAST xst (Array xs'), env')
    where
        extractType (TypedAST t _) = t
        foldEq (x:xs) = if all (==x) xs then Just x else Nothing


-- Interpreter (Temporary)

type Operator = (Type, Token) -> (Type, Token) -> (Type, Token)

-- Next step to improve this is to typeclass Tokens
numericOperator :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Operator
numericOperator f _ (IntegerType, IntegerT x) (IntegerType, IntegerT y)
    = (IntegerType, IntegerT $ f x y)
numericOperator _ f (FloatType, FloatT x) (FloatType, FloatT y)
    = (FloatType, FloatT $ f x y)
numericOperator f g (ArrayType t, ArrayT xs) y = (ArrayType t, ArrayT $ map snd xs'')
    where
        xs' = map (t,) xs
        xs'' = map (\x -> numericOperator f g x y) xs'
numericOperator f g x (ArrayType t, ArrayT ys) = (ArrayType t, ArrayT $ map snd ys'')
    where
        ys' = map (t,) ys
        ys'' = map (numericOperator f g x) ys'

operators :: Map.Map String Operator
operators =
    Map.fromList
        [("+", numericOperator (+) (+))
        ,("*", numericOperator (*) (*))
        ]

typedToken :: TypedAST -> (Type, Token)
typedToken (TypedAST t (Literal x)) = (t, x)

type EnvMap = Map.Map String (Type, Token)
type Env = [EnvMap]

interpret :: TypedAST -> Env -> (TypedAST, Env)
interpret (TypedAST _ (Ident (IdentT x))) env = 
    (TypedAST t (Literal tok), env)
    where (t, tok) = fromJust $ getEnv env x
interpret (TypedAST t (Array xs)) env =
    (TypedAST t (Literal (ArrayT xs'')), env')
    where
        (xs', envs) = unzip $ map (`interpret` env) xs
        xs'' = map (snd . typedToken) xs'
        env' = foldl1 mergeEnv envs
interpret (TypedAST _ (Expr x@(TypedAST _ (Ident (IdentT ident)))
                       AssignmentT y)) env =
    (TypedAST yt (Literal tok), env')
    where
        (x', _) = interpret x env
        (y'@(TypedAST yt (Literal tok)), yEnvH:yEnvT) = interpret y env
        env' = Map.insert ident (yt, tok) yEnvH:yEnvT

interpret (TypedAST _ (Expr x (OperatorT op) y)) env =
    (TypedAST t (Literal z), env')
    where
        (x', xEnv) = interpret x env
        x'' = typedToken x'
        (y', yEnv) = interpret y env
        y'' = typedToken y'
        env' = mergeEnv xEnv yEnv
        (t, z) = (operators Map.! op) x'' y''
interpret other env = (other, env)

-- data SSA = SSA ()
-- data ASM = ASM ()

showResult :: TypedAST -> String
showResult (TypedAST _ (Literal (IntegerT x))) = show x
showResult (TypedAST _ (Literal (FloatT x)))   = show x
showResult (TypedAST _ (Literal (StringT x)))  = show x
showResult (TypedAST t (Literal (ArrayT xs)))  =
    unwords $ map (showResult . TypedAST t . Literal) xs

emptySimpleEnv :: SimpleEnv = [Map.empty]
emptyEnv :: Env = [Map.empty]

ivy :: String -> SimpleEnv -> Env -> Maybe (TypedAST, SimpleEnv, Env)
ivy input simpleEnv env = do
    (ast, simpleEnv') <- ((`check` simpleEnv) <=< parse <=< tokens <=< lexemes) input
    let (result, env') = interpret ast env
    Just (result, simpleEnv', env')

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine

main :: IO ()
main = loop emptySimpleEnv emptyEnv
    where
        loop simpleEnv env = do
            line <- prompt " ; "
            let Just (ast, simpleEnv', env') = ivy line simpleEnv env
            _ <- putStrLn $ showResult ast
            loop simpleEnv' env'

