{-|
Module      : Tiger.Temporaries
Description : This library defines the functions and types for performing escape analysis.
Copyright   : (c) Sherman Pay, 2015
License     : 
Maintainer  : shermanpay1991@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Tiger.Escape where

import qualified Data.IORef as IORef
import qualified Tiger.AST as AST
import qualified Tiger.Symbol as Sym

type Depth = Int
type EscEnv = Sym.Table (Depth, (IORef.IORef Bool))

traverseVar :: EscEnv -> Depth -> AST.Expr -> IO () 
traverseVar _ _ _ = return ()

traverseExpr :: EscEnv -> Depth -> AST.Expr -> IO () 
traverseExpr _ _ _ = return ()

traverseDecl :: EscEnv -> Depth -> AST.Decl -> IO () 
traverseDecl _ _ _ = return ()

findEscape :: prog AST.Prog -> IO ()
findEscape _ = return ()
