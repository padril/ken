module Compiler (compile, defaultRegisters, asmPrefix, asmSuffix, Line) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import IR (Var, SSA(..), Instr(..), Env)
import Checker (SimpleEnv)
import Evaluator (Token(..))

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
compile (reg, lines) (Assign var IPrintInstr [xv]) =
    (reg', reverse
           ["    push rdi"
           ,"    mov rdi, " ++ xr
           ,"    call iprintln"
           ,"    pop rdi"]
           ++ lines)
    where
        xr = head $ lookupKey (Just xv) reg
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

