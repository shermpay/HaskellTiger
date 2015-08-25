{-|
Module      : Tiger.Semantics
Description : This library performs various semantic analyses on Tiger AST 
Copyright   : (c) Sherman Pay, 2015
License     : 
Maintainer  : shermanpay1991@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
-- TODO: Improved error messages
module Tiger.Semantic where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.IORef as IORef
import qualified Data.Unique as Uniq
import qualified Control.Monad as Monad
import qualified Tiger.AST as AST
import qualified Tiger.Symbol as Sym
import qualified Tiger.Type as Ty 
import qualified Tiger.TypeChecker as TC
import qualified Tiger.Escape as Esc

-- | Initialize SymTable with primitive types
initTypeSymTable :: Sym.Table Ty.Type
initTypeSymTable = Sym.Table Ty.primitivesMap

initValSymTable :: Sym.Table Ty.Type
initValSymTable = Sym.Table Ty.builtinsMap

-- | SymTable encapsulates all possible SymTables required to compile a Tiger program
data SymTables = SymTables { valEnv :: Sym.Table Ty.Type
                           , typEnv :: Sym.Table Ty.Type
                           , escEnv :: Esc.EscEnv } deriving (Show)

-- | Create and initialize SymTables
newSymTables :: SymTables
newSymTables = SymTables { valEnv=initValSymTable
                         , typEnv=initTypeSymTable
                         , escEnv=Sym.Table Map.empty }

progPass :: SymTables -> AST.Prog -> IO ()
progPass tabs (AST.Prog prog) = do
  TC.checkExpr (TC.SymTables { TC.valEnv=valEnv tabs
                             , TC.typEnv=typEnv tabs }) prog 
  return ()
