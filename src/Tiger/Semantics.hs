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
            
primitivesMap :: Map.Map Sym Type
primitivesMap = Map.fromList [
                  (Sym "int", TInt)
                , (Sym "string", TString)]

typeFromAST ::  SymTable -> AST.Type -> Type
typeFromAST (SymTable tab) (AST.Type ident) =  case Map.lookup (Sym ident) tab of
                                                 Just x -> x
                                                 Nothing -> TName (Sym ident) Nothing
typeFromAST tab (AST.RecordType recty) = TRecord $ 
                                     map (\(ident, ty) -> (Sym ident, typeFromAST tab ty))
                                         recty
typeFromAST tab (AST.ArrayType arrty) = TArray $ typeFromAST tab arrty
                                   
newtype Sym = Sym String deriving (Eq, Ord, Show)

newtype SymTable = SymTable (Map.Map Sym Type) deriving (Show)

addSym :: Sym -> Type -> SymTable -> SymTable
addSym sym ty (SymTable tab) =
    SymTable $ Map.insert sym ty tab
                
addString :: String -> Type -> SymTable -> SymTable
addString var = addSym (Sym var)

emptySymTable :: SymTable
emptySymTable = SymTable $ Map.empty

initTypeSymTable :: SymTable
initTypeSymTable = SymTable primitivesMap

data SymTables = SymTables { valEnv :: SymTable
                           , typEnv :: SymTable } deriving (Show)

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
               

addDecls :: SymTables -> [AST.Decl] -> SymTables
addDecls = List.foldl' addDecl

newSymTables :: SymTables
newSymTables = SymTables { valEnv=emptySymTable
                         , typEnv=initTypeSymTable }

buildSymTables :: AST.Prog -> SymTables -> SymTables
buildSymTables (AST.Prog prog) tabs = buildTables prog tabs

buildTables :: AST.Expr -> SymTables -> SymTables
buildTables AST.Let { AST.letDecls=decls
                    , AST.letBody=body } tabs = addDecls tabs decls
buildTables _ tabs = tabs
