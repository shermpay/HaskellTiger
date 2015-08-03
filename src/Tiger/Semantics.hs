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
module Tiger.Semantics where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Tiger.AST as AST

-- Sym Tables:
--      String -> Type (Variable/Functions -> Type)
--      String -> Type (Type -> Type)
data Type = TInt
          | TString
          | TRecord [(Sym, Type)]
          | TArray Type
          | TNil
          | TUnit
          | TFunc [Type] (Maybe Type)
          | TName Sym (Maybe Type)
            deriving (Show)
            
-- | Map of primitive types
-- Corresponding data type should be declared in Type
primitivesMap :: Map.Map Sym Type
primitivesMap = Map.fromList [
                  (Sym "int", TInt)
                , (Sym "string", TString)]

-- | Takes an AST.Type and converts it to a Type
typeFromAST ::  SymTable -> AST.Type -> Type
typeFromAST (SymTable tab) (AST.Type ident) =  case Map.lookup (Sym ident) tab of
                                                 Just x -> x
                                                 Nothing -> TName (Sym ident) Nothing
typeFromAST tab (AST.RecordType recty) = TRecord $ 
                                     map (\(ident, ty) -> (Sym ident, typeFromAST tab ty))
                                         recty
typeFromAST tab (AST.ArrayType arrty) = TArray $ typeFromAST tab arrty
                                   
-- | Sym represents a Symbol in the SymTable
newtype Sym = Sym String deriving (Eq, Ord, Show)

-- | A SymTable is a SymbolTable that is generally use for mapping Sym to Type
newtype SymTable = SymTable (Map.Map Sym Type) deriving (Show)

-- | Add a Sym => Type mapping in a SymTable
addSym :: Sym -> Type -> SymTable -> SymTable
addSym sym ty (SymTable tab) =
    SymTable $ Map.insert sym ty tab
                
-- | Add a String => Type mapping in a SymTable
addString :: String -> Type -> SymTable -> SymTable
addString var = addSym (Sym var)

-- | Creates an empty SymTable
emptySymTable :: SymTable
emptySymTable = SymTable $ Map.empty

-- | Initialize SymTable with primitive types
initTypeSymTable :: SymTable
initTypeSymTable = SymTable primitivesMap

-- | SymTable encapsulates all possible SymTables required to compile a Tiger program
data SymTables = SymTables { valEnv :: SymTable
                           , typEnv :: SymTable } deriving (Show)

-- | Add a AST.Decl to SymTables
addDecl :: SymTables -> AST.Decl -> SymTables
addDecl SymTables { valEnv=tab
                  , typEnv=t }
            (AST.VarDecl
                    { AST.varName=vName
                    , AST.varType=mVarType
                    , AST.varExpr=_ }) =
            SymTables { valEnv=case mVarType of
                                 Just vType -> addString vName (typeFromAST t vType) tab
                                 Nothing -> tab
                      , typEnv=t }
addDecl SymTables { valEnv=t
                  , typEnv=tab }
            (AST.TypeDecl ident tyid) =
        SymTables { valEnv=t
                  , typEnv=addString ident (typeFromAST tab tyid) tab }
addDecl SymTables { valEnv=tab
                  , typEnv=t }
            (AST.FunctionDecl funcTy _) =
         case funcTy of
           AST.FuncType ident argTy retTy ->
               SymTables { valEnv=addString ident
                                   (TFunc (map (\(_, x) -> typeFromAST t x) argTy)
                                              (Just $ typeFromAST t retTy))
                                   tab
                         , typEnv=t }
           AST.ProcType ident argTy ->
               SymTables { valEnv=addString ident
                                   (TFunc (map (\(_, x) -> typeFromAST t x) argTy)
                                          Nothing)
                                   tab
                         , typEnv=t }
               

-- | Add a list of Decls to SymTables
addDecls :: SymTables -> [AST.Decl] -> SymTables
addDecls = List.foldl' addDecl

-- | Create and initialize SymTables
newSymTables :: SymTables
newSymTables = SymTables { valEnv=emptySymTable
                         , typEnv=initTypeSymTable }

-- | Build SymTables given a Prog
buildSymTables :: AST.Prog -> SymTables -> SymTables
buildSymTables (AST.Prog prog) tabs = buildTables prog tabs

-- | Build SymTables from Let expressions
buildTables :: AST.Expr -> SymTables -> SymTables
buildTables AST.Let { AST.letDecls=decls
                    , AST.letBody=body } tabs = addDecls tabs decls
buildTables _ tabs = tabs
