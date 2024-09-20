import Control.Applicative
import Control.Monad
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import System.IO
import qualified System.Environment as Environment


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

optC :: Consumer i o -> Consumer i (Maybe o)
optC lexer@(Consumer l) = Consumer $ \input ->
    case l input of
        Just (x, input') -> Just (Just x, input)
        Nothing -> Just (Nothing, input)

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


-- Evaluator

data Token
    = IntegerT Int
    | OpenParenT
    | CloseParenT
    | AssignmentT
    | EndStatementT
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
evaluate (SeperatorL ";")  = Just EndStatementT
evaluate _                 = Nothing

tokens :: [Lexeme] -> Maybe [Token]
tokens = traverse evaluate


-- Parser

data AST a
    = EmptyAST
    | Literal Token
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

baseASTP :: Parser BaseAST
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
    = NeverType
    | IntegerType
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
check (BaseAST EmptyAST) env =
    Just (TypedAST NeverType EmptyAST, env)
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

operators :: Map.Map (String, Type, Type) Instr
operators =
    Map.fromList
        [(("+", IntegerType, IntegerType), IAddInstr)
        ,(("*", IntegerType, IntegerType), IAddInstr)
        ]

typedASTType :: TypedAST -> Type
typedASTType (TypedAST t _) = t

type EnvMap = Map.Map Var Token
type Env = [EnvMap]

data Var = Label Int | IdentV String
    deriving (Eq, Show, Ord)
nextVar :: Var -> Var
nextVar (Label v) = Label $ v + 1

data Instr = AliasInstr | CallInstr | IAddInstr
    deriving Show
data SSA = Noop | Copy Var Token | Assign Var Instr [Var]
    deriving Show


--                                    ret  next
makeSSA :: TypedAST -> Var -> ([SSA], Var, Var)
makeSSA (TypedAST _ EmptyAST) var = 
    ([Noop], var, var)
makeSSA (TypedAST _ (Literal tok)) var = 
    ([Copy var tok], var, nextVar var)
makeSSA (TypedAST _ (Ident (IdentT x))) var = 
    ([Noop], IdentV x, var)
makeSSA (TypedAST t (Array xs)) var = undefined
makeSSA (TypedAST _ (Expr (TypedAST _ (Ident (IdentT x)))
                       AssignmentT y)) var =
    (Assign ident AliasInstr [ret]:ySSA, ident, next)
    where
        ident = IdentV x
        (ySSA, ret, next) = makeSSA y var
makeSSA (TypedAST _ (Expr x (OperatorT op) y)) var =
    (Assign next' instr [xRet, yRet]:ySSA ++ xSSA, next', nextVar next')
    where
        (xSSA, xRet, next) = makeSSA x var
        (ySSA, yRet, next') = makeSSA y next
        xt = typedASTType x
        yt = typedASTType y
        instr = operators Map.! (op, xt, yt)

-- interpret :: (Var, EnvMap) -> SSA -> (Var, EnvMap)
-- interpret (var, env) Noop = (var, env)
-- interpret (_, env) (Copy var value) = (var, env')
--     where env' = Map.insert var value env
-- interpret (_, env) (Assign var IAddInstr [xv, yv]) = (var, env')
--     where
--         (IntegerT x) = env Map.! xv
--         (IntegerT y) = env Map.! yv
--         z = IntegerT $ x + y
--         env' = Map.insert var z env

type Line = String
type Registers = Map.Map String (Maybe Var)

nextAvailable :: Registers -> String
nextAvailable = fromJust . Map.foldrWithKey f Nothing
    where
        f k Nothing Nothing = Just k
        f _ _ acc = acc

lookupKey :: Eq v => v -> Map.Map k v -> [k]
lookupKey val = Map.foldrWithKey go [] where
    go key value found =
        if value == val
        then key:found
        else found

compileToken :: Token -> String
compileToken (IntegerT x) = show x

compile :: (Registers, [Line]) -> SSA -> (Registers, [Line])
compile (reg, lines) Noop = (reg, "":lines)
compile (reg, lines) (Copy var tok) =
    (reg', ("    mov " ++ nextReg ++ ", " ++ value):lines)
    where
        nextReg = nextAvailable reg
        reg' = Map.adjust (const $ Just var) nextReg reg
        value = compileToken tok
compile (reg, lines) (Assign var IAddInstr [xv, yv]) =
    (reg', reverse
           ["    mov " ++ zr ++ ", " ++ xr
           ,"    add " ++ zr ++ ", " ++ yr]
           ++ lines)
    where
        xr = head $ lookupKey (Just xv) reg
        yr = head $ lookupKey (Just yv) reg
        zr = nextAvailable reg
        reg' = Map.adjust (const $ Just var) zr reg

-- data ASM = ASM ()

showResult :: Token -> String
showResult (IntegerT x) = show x
showResult (FloatT x)   = show x
showResult (StringT x)  = show x
showResult (ArrayT xs)  = undefined

emptySimpleEnv :: SimpleEnv = [Map.empty]
emptyEnv :: Env = [Map.empty]

defaultRegisters =
    Map.fromList [("rax", Nothing)
                 ,("rbx", Nothing)
                 ,("rcx", Nothing)
                 ,("rdx", Nothing)
                 ,("rdi", Nothing)
                 ,("rsi", Nothing)
                 ,("r8",  Nothing)
                 ,("r9",  Nothing)
                 ,("r10", Nothing)
                 ,("r11", Nothing)
                 ,("r12", Nothing)
                 ,("r13", Nothing)
                 ,("r14", Nothing)
                 ,("r15", Nothing)
                 ]

asmPrefix :: [Line]
asmPrefix =
    ["default rel"
    ,"bits 64"
    ,""
    ,"global _start"
    ,""
    ,"section .text"
    ,""
    ,"%include\"lib.s\""
    ,""
    ,"_start:"
    ]

asmSuffix :: [Line]
asmSuffix = ["    call exit"]

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy v = filter (not . null) . foldr f [[]]
    where
        f x acc | x == v = []:acc
        f x (h:t) = (x:h):t

ken :: String -> Maybe [Line]
ken input = do
    programTokens <- (tokens <=< lexemes) input
    let programStatements = splitBy EndStatementT programTokens
    (asm, _) <- foldM fold ([], emptySimpleEnv) programStatements
    Just $ asmPrefix ++ asm ++ asmSuffix
    where
        fold :: ([Line], SimpleEnv) -> [Token] -> Maybe ([Line], SimpleEnv)
        fold (lines, env) statement = do
            (ast, env') <- ((`check` env) <=< parse) statement
            let (ssa, _, _) = makeSSA ast (Label 0)
            let ssa' = reverse ssa
            -- let (var, env) = foldl interpret (Label 0, Map.empty) ssa'
            -- Map.lookup var env
            let (reg, lines) = foldl compile (defaultRegisters, []) ssa'
            let lines' = reverse lines
            Just (lines', env')

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine

-- main :: IO ()
-- main = loop emptySimpleEnv
--     where
--         loop simpleEnv = do
--             line <- prompt " ; "
--             let Just result = ken line simpleEnv
--             _ <- putStrLn $ foldl1 (++) $ map (++"\n") result
--             loop emptySimpleEnv

main :: IO ()
main = do
    [infile, outfile] <- Environment.getArgs
    handle <- openFile infile ReadMode
    contents <- hGetContents handle
    let asm = fromJust $ ken contents
    writeFile outfile $ unlines asm

