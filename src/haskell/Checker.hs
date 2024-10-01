module Checker (check, Type(..), TypedAST(..), SimpleEnv) where

import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Parser (AST(..), BaseAST(..))
import Evaluator (Token(..))

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
check (BaseAST (Print x)) env = do
    (x'@(TypedAST t _), env') <- check x env
    return (TypedAST t (Print x'), env')
check (BaseAST (Array xs)) env = do
    (xs', envs) <- mapAndUnzipM (`check` env) xs
    let xts = map extractType xs'
    let env' = foldl1 mergeEnv envs
    xst <- ArrayType <$> foldEq xts
    Just (TypedAST xst (Array xs'), env')
    where
        extractType (TypedAST t _) = t
        foldEq (x:xs) = if all (==x) xs then Just x else Nothing

