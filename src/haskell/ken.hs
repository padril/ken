import Lexer (lexemes)
import Parser (parse)
import Evaluator (tokens, Token(..))
import Checker (check, SimpleEnv)
import Util (splitBy)
import Compiler (compile, defaultRegisters, Line, asmPrefix, asmSuffix)
import IR (makeSSA, Var(..))

import qualified Data.Map as Map
import Control.Monad
import qualified System.Environment as Environment
import System.IO
import Data.Maybe (fromJust)

ken :: String -> Maybe [Line]
ken input = do
    programTokens <- (tokens <=< lexemes) input
    let programStatements = splitBy EndStatementT programTokens
    (asm, _) <- foldM fold ([], [Map.empty]) programStatements
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

main :: IO ()
main = do
    [infile, outfile] <- Environment.getArgs
    handle <- openFile infile ReadMode
    contents <- hGetContents handle
    let asm = fromJust $ ken contents
    writeFile outfile $ unlines asm

