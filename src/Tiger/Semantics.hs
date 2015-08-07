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
module Tiger.Semantics where

import qualified Data.Map as Map
import qualified Data.List as List
import Text.Parsec.Pos (SourcePos, newPos, initialPos)

import qualified Tiger.AST as AST

-- Sym Tables:
--      String -> Type (Variable/Functions -> Type)
--      String -> Type (Type -> Type)
-- TODO: Type equality
data Type = TInt
          | TString
          | TRecord [(Sym, Type)]
          | TArray Type
          | TNil
          | TUnit
          | TFunc [Type] (Maybe Type)
          | TName Sym (Maybe Type)
            deriving (Show, Eq)
            
-- | Map of primitive types
-- Corresponding data type should be declared in Type
primitivesMap :: Map.Map Sym Type
primitivesMap = Map.fromList [
                  (Sym "int", TInt)
                , (Sym "string", TString)]

-- | Takes an AST.Type and converts it to a Type
-- Returns TName Sym Nothing if cannot find corresponding type
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

getSym :: Sym -> SymTable -> Type
getSym s (SymTable m) = case Map.lookup s m of
                             Just t -> t
                             Nothing -> error $ "Unable to obtain type for " ++ (show s)

getString :: String -> SymTable -> Type
getString s = getSym (Sym s)

lookupSym :: Sym -> SymTable -> Maybe Type
lookupSym s (SymTable m) = Map.lookup s m

lookupString :: String -> SymTable -> Maybe Type
lookupString s = lookupSym (Sym s)

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
addDecl tab@(SymTables { valEnv=ve
                       , typEnv=t })
            (AST.VarDecl
                    { AST.varName=vName
                    , AST.varType=mVarType
                    , AST.varExpr=expr
                    , AST.varPos=pos })
    = SymTables { valEnv=case existing of
                           Just ty -> if exTy == ty
                                      then case mVarType of
                                             Just vType -> checkAnn vType
                                             Nothing -> ve
                                     else newErr pos "Cannot assign incompatible types"
                           Nothing ->
                               case mVarType of
                                 Just vType -> checkAnn vType
                                 Nothing -> addString vName exTy ve
                , typEnv=t }
      where exTy = typeCheck tab expr
            existing = lookupString vName ve
            checkAnn annNode = if exTy == vt
                               then addString vName exTy ve
                               else newErr pos "Expr and ann type mismatch"
                                   where vt = (typeFromAST t annNode)
            
addDecl tab@(SymTables { valEnv=t
                       , typEnv=te })
            (AST.TypeDecl pos ident tyid)
    = SymTables { valEnv=t
                , typEnv=addString ident (typeFromAST te tyid) te }
addDecl tab@(SymTables { valEnv=ve
                       , typEnv=t })
            (AST.FunctionDecl pos funcTy body)
    = case funcTy of
           AST.FuncType ident argTy retNode ->
               SymTables { valEnv=addString ident
                                   (if retTy == (typeCheck tab body)
                                    then (TFunc (map (\(_, x) -> typeFromAST t x) argTy)
                                              (Just retTy ))
                                    else newErr pos "Function body return type mismatch")
                                   ve
                         , typEnv=t }
                   where retTy = typeFromAST t retNode
           AST.ProcType ident argTy ->
               SymTables { valEnv=addString ident
                                   (if TUnit == (typeCheck tab body)
                                    then (TFunc (map (\(_, x) -> typeFromAST t x) argTy)
                                                Nothing)
                                    else newErr pos "Procedure body cannot have value")
                                   ve
                         , typEnv=t }
               

-- | Add a list of Decls to SymTables
addDecls :: SymTables -> [AST.Decl] -> SymTables
addDecls = List.foldl' addDecl

-- | Create and initialize SymTables
newSymTables :: SymTables
newSymTables = SymTables { valEnv=emptySymTable
                         , typEnv=initTypeSymTable }

-- | Analyze and build SymTables given a Prog
analyze :: AST.Prog -> SymTables -> Type
analyze (AST.Prog prog) tabs = typeCheck tabs prog 

newErr :: SourcePos -> String -> a
newErr p s = error $ (show p) ++ ": " ++ s

typeCheckList :: SymTables -> [AST.Expr] -> Type
typeCheckList tab es = if List.null es
                       then TUnit
                       else last $ map (typeCheck tab) es

typeCheck :: SymTables -> AST.Expr -> Type
typeCheck tab (AST.IdExpr _ idt) = getString idt $ valEnv tab
typeCheck tab (AST.FieldDeref pos re (AST.IdExpr _ field)) =
    case typeCheck tab re of
      -- TODO: Assuming same order now
      TRecord ty -> case List.lookup (Sym field) ty of
                      Just x -> x
                      Nothing -> newErr pos "record does not have field"
      _ -> newErr pos "Deref requires record type"
                                                        
typeCheck tab (AST.ArraySub pos arr e)
    = case typeCheck tab arr of
        TArray ty -> if typeCheck tab e == TInt
                     then ty
                     else newErr pos "Array subscript not int"
        _ -> newErr pos "Subscript requires array type"
typeCheck _ (AST.Nil _) = TNil
typeCheck _ (AST.IntConst _ _) = TInt
typeCheck _ (AST.StringLit _ _) = TString
typeCheck tab (AST.SeqExpr _ es) = typeCheckList tab es
typeCheck tab (AST.Neg pos e) = case (typeCheck tab e) of
                                  TInt -> TInt
                                  _ -> newErr pos "Cannot negate non-number"
typeCheck tab@(SymTables { valEnv=vTab }) (AST.Call pos f args) =
    if paramTy == argTy
    then retTy
    else newErr pos ("argument types do not match" ++ "\nformals: " ++ show paramTy ++ 
             "\nargs:    " ++ show argTy)
    where argTy = map (typeCheck tab) args
          TFunc paramTy ret = getString f vTab
          retTy = case ret of
                    Just x -> x
                    Nothing -> TUnit
typeCheck tab (AST.InfixOp pos _ l r) =
    case (lty, rty) of
      (TInt, TInt) -> TInt
      _ -> newErr pos "Cannot apply binary op to non-number"
    where lty = typeCheck tab l
          rty = typeCheck tab r
typeCheck tab AST.NewArr { AST.arrayType=(AST.Type idt)
                         , AST.arraySize=arrSize
                         , AST.arrayInit=arrInit
                         , AST.arrayPos=pos } =
    case (typeCheck tab arrSize) of
      TInt -> if  arrTy == initTy
              then TArray initTy
              else newErr pos "Array initialization expression type mismatch"
          where initTy = typeCheck tab arrInit
                arrTy = getString idt (typEnv tab)
      _ -> newErr pos "Array size has to be integer"
typeCheck tab (AST.NewRec pos (AST.Type idt) fields)
    -- TODO: Currently assumes same ordering
    = if expTy == actTy
      then actTy
      else newErr pos "Record initialization type mismatch"
          where expTy = getString idt (typEnv tab) 
                actTy = TRecord $ map (\(s, expr) -> (Sym s, typeCheck tab expr)) fields
typeCheck tab (AST.Assign pos le re) = if typeCheck tab le == typeCheck tab re
                                       then TUnit
                                       else newErr pos "Assignment type mismatch"
typeCheck tab AST.If { AST.ifTest=t
                     , AST.thenExpr=th
                     , AST.elseExpr=el
                     , AST.ifPos=pos}
    = case typeCheck tab t of
        TInt -> case el of
                  Just e ->
                      if elTy == (typeCheck tab th)
                      then elTy
                      else newErr pos "then else branch types mismatch"
                      where elTy = (typeCheck tab e)
                  Nothing -> case typeCheck tab th of
                               TUnit -> TUnit
                               _ -> newErr pos "if-then cannot have value"
        _ -> newErr pos "if test type not int"
typeCheck tab AST.While { AST.whileTest=t
                        , AST.whileBody=b
                        , AST.whilePos=pos }
    = case (typeCheck tab t, typeCheck tab b) of
        (TInt, TUnit) -> TUnit
        (_, TUnit) -> newErr pos "while test type not int"
        (TInt, _) -> newErr pos "while body cannot have value"
        (_, _) -> newErr pos "while test type not int and body cannot have value"
typeCheck tab AST.For { AST.forVarName=_
                      , AST.forVarExpr=ve
                      , AST.toExpr=vt
                      , AST.doExpr=d
                      , AST.forPos=pos }
    = case (typeCheck tab ve, typeCheck tab vt) of
        (TInt, TInt) -> case (typeCheck tab d) of
                          TUnit -> TUnit
                          _ -> newErr pos "For loop body cannot have value"
        _ -> newErr pos "for var type not int"
typeCheck _ (AST.Break _) = TUnit
typeCheck tab (AST.Let { AST.letDecls=decls
                       , AST.letBody=body }) =  typeCheckList (buildTable decls tab) body

-- | Build SymTables from Decls
buildTable :: [AST.Decl] -> SymTables -> SymTables
buildTable decls tabs = addDecls tabs decls
