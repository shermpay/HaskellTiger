-- Sherman Pay Jing Hao
-- Tuesday, 07. April 2015
-- Main Driver to Tiger Compiler

import qualified System.Environment as Env
import qualified System.IO as IO
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )

import qualified Tiger.Parser as Parser
    
version = "0.1"
data Flag = Parse IO.FilePath
          | Version
          | Help
          deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['p'] ["parse"] (ReqArg Parse "FILE") "parse FILE"
    , Option ['h'] ["help"]  (NoArg Help) "Prints this usage string" ]
    
parseFile :: String -> IO Parser.Prog
parseFile fileName = do
    sourceCode <- IO.readFile fileName
    return $ Parser.parseProg fileName sourceCode

handleOpt :: Flag -> IO ()
handleOpt (Parse f) = do
  prog <- parseFile f
  putStrLn $ show prog
  return ()

handleOpt Help = usage
         
usage :: IO ()
usage = putStr $ usageInfo "Usage: tiger [options] [args]" options

main :: IO ()
main = do
  args <- Env.getArgs
  case getOpt Permute options args of
    (optArgs, _, []) -> case optArgs of 
                          arg:_ -> handleOpt $ arg
                          [] -> usage
    (_, _, err) -> do { putStr $ head err; usage }
