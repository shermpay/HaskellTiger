-- Sherman Pay Jing Hao
-- Tuesday, 07. April 2015
-- Main Driver to Tiger Compiler

import qualified System.Environment as Env
import qualified System.IO as IO
import System.Console.GetOpt
import System.Exit

import qualified Tiger.AST as AST
import qualified Tiger.Parser as Parser
    
-- | Command line flags
data Flag = Parse IO.FilePath
          | Version
          | Help
          deriving Show

-- | Command line options
options :: [OptDescr Flag]
options =
    [ Option ['p'] ["parse"] (ReqArg Parse "FILE") "parse FILE"
    , Option ['h'] ["help"]  (NoArg Help) "Prints this usage string" ]
    
-- | Parses a file given a String representing the filename
parseFile :: String -> IO AST.Prog
parseFile fileName = do
    sourceCode <- IO.readFile fileName
    return $ Parser.parseProg fileName sourceCode

-- | Handle various command line options
handleOpt :: Flag -> IO ()
handleOpt (Parse f) = do
  prog <- parseFile f
  putStrLn $ show prog
  return ()
handleOpt Help = usage
         
-- | Print usage string
usage :: IO ()
usage = putStr $ usageInfo "Usage: tiger [options] [args]" options

main :: IO ()
main = do
  args <- Env.getArgs
  case getOpt Permute options args of
    (optArgs, _, []) -> do  
         case optArgs of 
           arg:_ -> handleOpt $ arg
           [] -> usage
         System.Exit.exitSuccess
    (_, _, err) -> do { putStr $ head err; usage; System.Exit.exitFailure }
