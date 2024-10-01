module IR (makeSSA, Var(..), SSA(..), Instr(..), Env, EnvMap) where

import qualified Data.Map as Map
import Parser (AST(..))
import Checker (Type(..), TypedAST(..))
import Evaluator (Token(..))

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

data Instr = AliasInstr | CallInstr | IAddInstr | IPrintInstr
    deriving Show
data SSA
    = Noop
    | Copy Var Token
    | Assign Var Instr [Var]
    deriving Show


--                     curr           ret  next
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
makeSSA (TypedAST IntegerType (Print x )) var =
    (Assign next IPrintInstr [x']:ssa, next, nextVar next)
    where
        (ssa, x', next) = makeSSA x var
        xt = typedASTType x

