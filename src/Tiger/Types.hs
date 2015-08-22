{-|
Module      : Tiger.Types
Description : This module specifies the Tiger Language type system
Copyright   : (c) Sherman Pay, 2015
License     : 
Maintainer  : shermanpay1991@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
-- TODO: Improved error messages
module Tiger.Types where
import qualified Data.Map as Map
import qualified Data.IORef as IORef
import qualified Data.Unique as Uniq

import qualified Tiger.AST as AST
import qualified Tiger.Symbol as Sym

data Type = TInt
          | TString
          | TRecord [(Sym.Sym, Type)] Uniq.Unique
          | TArray Type Uniq.Unique
          | TNil
          | TUnit
          | TFunc [Type] (Maybe Type)
          | TName Sym.Sym (IORef.IORef (Maybe Type))
          | TFail                -- Special type used in error case
            deriving (Show, Eq)
            -- deriving (Eq)

instance Show (IORef.IORef a) where
    show x = "(*)"

instance Show Uniq.Unique where
    show x = "(" ++ (show $ Uniq.hashUnique x) ++ ")"

(=::) :: Type -> Type -> Bool
(=::) TNil (TRecord  _ _) = True
(=::) (TRecord _ _) TNil = True
(=::) (TRecord _ x) (TRecord _ y) = x == y
(=::) (TArray _ x) (TArray _ y) = x == y
(=::) TFail _ = True
(=::) _ TFail = True
(=::) t1 t2 = t1 == t2

tyListEq :: [Type] -> [Type] -> Bool
tyListEq [] [] = True
tyListEq (x:xs) (y:ys) = if x =:: y
                         then tyListEq xs ys
                         else False
            
resolveType :: Sym.Table Type -> Type -> IO Type
resolveType tab (TName s ref) = do
  ty <- IORef.readIORef ref
  case ty of
    Just syn -> resolveType tab syn
    Nothing -> error ("Cannot resolve type " ++ (show s))
resolveType _ primTy = return primTy
            
-- | Takes an AST.Type and converts it to a Type
-- Returns TName Sym Nothing if cannot find corresponding type
typeFromAST :: Sym.Table Type -> AST.Type -> IO Type
typeFromAST (Sym.Table tab) (AST.Type ident) = case Map.lookup (Sym.Sym ident) tab of
                                                Just ty -> return ty
                                                Nothing -> do
                                                    ref <- IORef.newIORef Nothing
                                                    return $ TName (Sym.Sym ident) ref
typeFromAST tab (AST.RecordType recty) = do
  unique <- Uniq.newUnique 
  return $ TRecord
             (map (\(ident, tyId) -> (Sym.Sym ident, Sym.getString tyId tab)) recty)
             unique
typeFromAST tab (AST.ArrayType arrty) = do
  unique <- Uniq.newUnique 
  return $ TArray (Sym.getString arrty tab) unique

-- | Map of primitive types
-- Corresponding data type should be declared in Type
primitivesMap :: Map.Map Sym.Sym Type
primitivesMap = Map.fromList [ (Sym.Sym "int", TInt)
                             , (Sym.Sym "string", TString)]

-- | Map of builtin functions
builtinsMap :: Map.Map Sym.Sym Type
builtinsMap = Map.fromList [
               (Sym.Sym "print", TFunc [TString] Nothing)
              , (Sym.Sym "getchar", TFunc [] $ Just TString)
              , (Sym.Sym "ord", TFunc [TString] $ Just TInt)
              , (Sym.Sym "flush", TFunc [] Nothing)
              , (Sym.Sym "chr", TFunc [TInt] $ Just TString)
              , (Sym.Sym "size", TFunc [TString] $ Just TInt)
              , (Sym.Sym "substring", TFunc [TString, TInt, TInt] $ Just TString)
              , (Sym.Sym "concat", TFunc [TString, TString] $ Just TString)
              , (Sym.Sym "not", TFunc [TInt] $ Just TInt)
              , (Sym.Sym "exit", TFunc [TInt] Nothing) ] 


