-- Sherman Pay Jing Hao
-- Tuesday, 07. April 2015
-- Main Driver to Tiger Compiler

import qualified System.Environment as Env
import qualified System.IO as IO

import qualified Tiger.Parser as Parser
    
parseFile :: String -> IO Parser.Prog
parseFile fileName = do
    sourceCode <- IO.readFile fileName
    return $ Parser.parseProg fileName sourceCode

main :: IO Parser.Prog
main = do
  args <- Env.getArgs
  parseFile (args !! 0)
