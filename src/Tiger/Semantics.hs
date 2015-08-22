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
import qualified Data.Either as Either
import qualified Data.IORef as IORef
import qualified Data.Unique as Uniq
import qualified Control.Monad as Monad
import qualified Control.Exception as Except
import qualified System.IO.Unsafe as Unsafe

import Text.Parsec.Pos (SourcePos, newPos, initialPos)

import qualified Tiger.AST as AST

-- Sym Tables:
--      String -> Type (Variable/Functions -> Type)
--      String -> Type (Type -> Type)
-- TODO: Type equality
data Type = TInt
          | TString
          | TRecord [(Sym, Type)] Uniq.Unique
          | TArray Type Uniq.Unique
          | TNil
          | TUnit
          | TFunc [Type] (Maybe Type)
          | TName Sym (IORef.IORef (Maybe Type))
          | TFail                -- Special type used in error case
            deriving (Show, Eq)
            -- deriving (Eq)

instance Show (IORef.IORef a) where
    show x = "(*)"

instance Show Uniq.Unique where
    show x = "(" ++ (show $ Uniq.hashUnique x) ++ ")"

resolveType :: SymTable -> Type -> IO Type
resolveType tab (TName s ref) = do
  ty <- IORef.readIORef ref
  case ty of
    Just syn -> resolveType tab syn
    Nothing -> error ("Cannot resolve type " ++ (show s))
resolveType _ primTy = return primTy

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
            
-- | Map of primitive types
-- Corresponding data type should be declared in Type
primitivesMap :: Map.Map Sym Type
primitivesMap = Map.fromList [(Sym "int", TInt)
                             , (Sym "string", TString)]

-- | Map of builtin functions
builtinsMap :: Map.Map Sym Type
builtinsMap = Map.fromList [ (Sym "print", TFunc [TString] Nothing)
                           , (Sym "getchar", TFunc [] $ Just TString)
                           , (Sym "ord", TFunc [TString] $ Just TInt)
                           , (Sym "flush", TFunc [] Nothing)
                           , (Sym "chr", TFunc [TInt] $ Just TString)
                           , (Sym "size", TFunc [TString] $ Just TInt)
                           , (Sym "substring", TFunc [TString, TInt, TInt] $ Just TString)
                           , (Sym "concat", TFunc [TString, TString] $ Just TString)
                           , (Sym "not", TFunc [TInt] $ Just TInt)
                           , (Sym "exit", TFunc [TInt] Nothing) ] 

-- | Takes an AST.Type and converts it to a Type
-- Returns TName Sym Nothing if cannot find corresponding type
typeFromAST ::  SymTable -> AST.Type -> IO Type
typeFromAST (SymTable tab) (AST.Type ident) = case Map.lookup (Sym ident) tab of
                                                Just ty -> return ty
                                                Nothing -> do
                                                    ref <- IORef.newIORef Nothing
                                                    return $ TName (Sym ident) ref
typeFromAST tab (AST.RecordType recty) = do
  unique <- Uniq.newUnique 
  return $ TRecord (map (\(ident, tyId) -> (Sym ident, getString tyId tab)) recty) unique
typeFromAST tab (AST.ArrayType arrty) = do
  unique <- Uniq.newUnique 
  return $ TArray (getString arrty tab) unique
                                   
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

initValSymTable :: SymTable
initValSymTable = SymTable builtinsMap

-- | Merge two SymTables with preference for the first one
mergeSymTable :: SymTable -> SymTable -> SymTable
mergeSymTable (SymTable m1) (SymTable m2) = SymTable $ Map.union m1 m2

-- | SymTable encapsulates all possible SymTables required to compile a Tiger program
data SymTables = SymTables { valEnv :: SymTable
                           , typEnv :: SymTable } deriving (Show)

-- | Add a AST.Decl to SymTables
addDecl :: SymTables -> AST.Decl -> IO SymTables
addDecl tab@(SymTables { valEnv=ve
                       , typEnv=t })
            (AST.VarDecl
                    { AST.varName=vName
                    , AST.varType=mAnnType
                    , AST.varExpr=expr
                    , AST.varPos=pos }) = do
              exTy <- typeCheck tab expr
              let checkAnn annNode = if exTy =:: vt
                                     then return $ addString vName exTy ve
                                     else putErr pos
                                              ("Expr and ann type mismatch" ++
                                               "\nExpr: " ++ show vt ++
                                               "\nAnn: " ++ show exTy)
                                              ve
                                         where vt = getString annNode t
              newValEnv <- case mAnnType of
                             Just annType -> checkAnn annType
                             Nothing -> case exTy of
                                          TNil -> putErr pos
                                                  "Nil expression needs to be annotated"
                                                  (addString vName exTy ve)
                                          _ -> return $ addString vName exTy ve
              return $ SymTables { valEnv=newValEnv, typEnv=t }
            
addDecl tab@(SymTables { valEnv=t
                       , typEnv=te })
            (AST.TypeDecl pos ident tyNode) = do
              ref <- IORef.newIORef Nothing
              let tempTe = addString ident (TName (Sym ident) ref) te
              ty <- typeFromAST tempTe tyNode >>= resolveType tempTe
              IORef.writeIORef ref (Just ty)
              -- myty <- IORef.readIORef ref
              -- putStrLn $ show myty
              -- putStrLn $ show tempTe
              return $SymTables { valEnv=t
                                , typEnv=
                                    case existing of
                                      Just _ -> newErr pos ("Redeclaring type: " ++ ident)
                                      Nothing -> addString ident ty te }
      where existing = lookupString ident te

addDecl tab@(SymTables { valEnv=ve
                       , typEnv=te })
            (AST.FunctionDecl pos funcTy body)
    -- TODO: Check if function is redeclaration
    = case funcTy of
           AST.FuncType ident paramTy retTyId -> do
                    bodyTy <- typeCheck env body
                    newValEnv <- addFunc ident fTy
                    if retTy =:: bodyTy
                    then return fTy
                    else putErr pos "Function body return type mismatch" fTy
                    return $
                           SymTables { valEnv=newValEnv, typEnv=te }
                   where vEnv = newEnv paramTy
                         env = SymTables { valEnv=addString ident fTy vEnv, typEnv=te }
                         retTy = getString retTyId te
                         fTy = TFunc (createFormals vEnv paramTy) (Just retTy)
           AST.ProcType ident paramTy -> do
                    bodyTy <- typeCheck env body
                    funTy <- if TUnit =:: bodyTy
                             then return fTy
                             -- TFail or fTy testcase40
                             else putErr pos "Procedure body cannot have value" fTy
                    newValEnv <- addFunc ident funTy
                    return $
                           SymTables { valEnv=newValEnv
                                     , typEnv=te }
                   where vEnv = newEnv paramTy
                         env = SymTables { valEnv=addString ident fTy vEnv, typEnv=te }
                         fTy = TFunc (createFormals vEnv paramTy) Nothing
      where addIdType env (idt, tyId) = case (lookupString idt env) of
                            Just _ -> newErr pos ("Invalid list of formals. " ++ idt)
                            Nothing -> addString idt (getString tyId te) env
            newEnv paramTy = mergeSymTable (List.foldl' addIdType emptySymTable paramTy)
                             ve
            createFormals env = map
                                (\(f, _) -> case lookupString f env of
                                              Just t -> t
                                              Nothing -> newErr pos "Type does not exist")
            addFunc idt func = case lookupString idt ve of
                                 Just _ -> putErr pos ("Redeclaring function: " ++ idt) ve
                                 Nothing -> return $ addString idt func ve

-- | Add a list of Decls to SymTables
addDecls :: SymTables -> [AST.Decl] -> IO SymTables
addDecls = Monad.foldM addDecl

-- | Create and initialize SymTables
newSymTables :: SymTables
newSymTables = SymTables { valEnv=initValSymTable
                         , typEnv=initTypeSymTable }

-- | Analyze and build SymTables given a Prog
analyze :: AST.Prog -> SymTables -> IO Type
analyze (AST.Prog prog) tabs = typeCheck tabs prog 

newErr :: SourcePos -> String -> a
newErr p s = error $ (show p) ++ ": " ++ s      

putErr :: SourcePos -> String -> a -> IO a
putErr p s v = (putStrLn $ (show p) ++ ": " ++ s) >> return v

typeCheckList :: SymTables -> [AST.Expr] -> IO Type
typeCheckList tab es = case es of
                         [] -> return TUnit
                         _ -> mapM (typeCheck tab) es >>= return . last

typeCheck :: SymTables -> AST.Expr -> IO Type
typeCheck SymTables  { valEnv=vt
                     , typEnv=tt } (AST.IdExpr pos idt)
    = case lookupString idt vt of
        Just t -> return t
        Nothing -> putErr pos ("Cannot resolve type for " ++ show idt) TFail
typeCheck tab (AST.FieldDeref pos re (AST.IdExpr _ field)) = do
  recTy <- typeCheck tab re
  case recTy of
    -- TODO: Assuming same order now
    TRecord fields _ -> case List.lookup (Sym field) fields of
                      Just (TName _ ref) -> do
                        maybeTy <- IORef.readIORef ref
                        case maybeTy of
                          Just ty -> return ty
                          Nothing -> putErr pos "record type invalid" TFail
                      Just ty -> return ty
                      Nothing -> newErr pos "record does not have field"
    _ -> newErr pos "Deref requires record type"
                                                        
typeCheck tab@(SymTables{typEnv=t}) (AST.ArraySub pos arr e) = do
  arrTy <- typeCheck tab arr
  sizeTy <- typeCheck tab e
  case arrTy of
    TArray ty _ -> if  sizeTy =:: TInt
                   then return ty
                   else newErr pos "Array subscript not int"
    _ -> newErr pos "Subscript requires array type"
typeCheck _ (AST.Nil _) = return TNil
typeCheck _ (AST.IntConst _ _) = return TInt
typeCheck _ (AST.StringLit _ _) = return TString
typeCheck tab (AST.SeqExpr _ es) = do
  typeCheckList tab es
typeCheck tab (AST.Neg pos e) = do
  expTy <- typeCheck tab e
  case expTy of
    TInt -> return TInt
    _ -> newErr pos "Cannot negate non-number"
typeCheck tab@(SymTables { valEnv=vTab
                         , typEnv=tTab }) (AST.Call pos f args) = do
  argTy <- mapM (typeCheck tab) args
  if paramTy `tyListEq` argTy
  then return retTy
  else newErr pos ("argument types do not match" ++ "\nformals: " ++ show paramTy ++ 
           "\nargs:    " ++ show argTy)
  where retTy = case ret of
                  Just x -> x
                  Nothing -> TUnit
        TFunc paramTy ret = case lookupString f vTab of
                              Just t -> t
                              Nothing ->
                                  newErr pos ("Cannot resolve func type " ++ show f)
typeCheck tab (AST.InfixOp pos op l r) = do
  lty <- typeCheck tab l
  rty <- typeCheck tab r
  case op of
      AST.Eq -> check lty rty
      AST.NotEq -> check lty rty
      _ -> case (lty, rty) of
             (TInt, TInt) -> return TInt
             _ -> newErr pos "Cannot apply binary op to non-number"
    where check t1 t2 = if t1 =:: t2
                        then return TInt
                        else newErr pos "Cannot compare two different types"
typeCheck tab AST.NewArr { AST.arrayType=(AST.Type idt)
                         , AST.arraySize=arrSize
                         , AST.arrayInit=arrInit
                         , AST.arrayPos=pos } = do
  initTy <- typeCheck tab arrInit
  arrSizeTy <- typeCheck tab arrSize
  case arrSizeTy of
    TInt -> if arrTy =:: initTy
            then return ty
            else newErr pos ("Array initialization expression type mismatch" ++
                             "\nexpected: " ++ show arrTy ++
                             "\nactual:   " ++ show initTy)
      where ty@(TArray arrTy _) = getString idt (typEnv tab)
    _ -> newErr pos "Array size has to be integer"
typeCheck tab@(SymTables { valEnv=_
                         , typEnv=tTab })
              (AST.NewRec pos (AST.Type idt) fields) = do
    -- TODO: Currently assumes same ordering
                actTys <- mapM liftTy fields
                let actTypes = map (\(_, t) -> t) actTys
                if expTypes `tyListEq` actTypes
                then return ty
                else newErr pos ("Record initialization type mismatch" ++
                                 "\nexpected: " ++ (show expTys) ++
                                 "\nactual:   " ++ (show actTys) ++
                                 "\nSymbols: " ++ show tab)
          where (ty, expTys) = case getString idt tTab of
                           -- (TRecord ty) -> TRecord $ map expandOnce ty
                                 recTy@(TRecord tys _) -> (recTy, tys)
                liftTy (s, expr) = typeCheck tab expr >>= (\x -> return (Sym s, x))
                expTypes = map (\(_, t) -> case t of
                                             TName (Sym s) _ -> getString s tTab
                                             _ -> t)
                           expTys
typeCheck tab@(SymTables { typEnv=tTab })
               (AST.Assign pos le re) = do
                 lTy <- typeCheck tab le
                 rTy <- typeCheck tab re
                 if lTy =:: rTy
                 then return TUnit
                 else newErr pos "Assignment type mismatch"
typeCheck tab@(SymTables { valEnv=_
                         , typEnv=tTab })
              (AST.If { AST.ifTest=t
                      , AST.thenExpr=th
                      , AST.elseExpr=el
                      , AST.ifPos=pos}) = do
                ifTy <- typeCheck tab t
                thTy <- typeCheck tab th
                case ifTy of
                  TInt -> case el of
                            Just e -> do 
                                    elTy <- typeCheck tab e
                                    if elTy =:: thTy
                                    then return elTy
                                    else newErr pos "then else branch types mismatch"
                            Nothing -> case thTy of
                                         TUnit -> return TUnit
                                         _ -> newErr pos "if-then cannot have value"
                  _ -> newErr pos "if test type not int"
typeCheck tab AST.While { AST.whileTest=t
                        , AST.whileBody=b
                        , AST.whilePos=pos } = do
  testTy <- typeCheck tab t
  bodyTy <- typeCheck tab b
  case (testTy, bodyTy) of
        (TInt, TUnit) -> return TUnit
        (_, TUnit) -> newErr pos "while test type not int"
        (TInt, _) -> newErr pos "while body cannot have value"
        (_, _) -> newErr pos "while test type not int and body cannot have value"
typeCheck tab AST.For { AST.forVarName=vn
                      , AST.forVarExpr=ve
                      , AST.toExpr=vt
                      , AST.doExpr=d
                      , AST.forPos=pos } = do
  veTy <- typeCheck tab ve
  vtTy <- typeCheck tab vt
  let newEnv = SymTables{valEnv=addString vn veTy (valEnv tab)
                        ,typEnv=(typEnv tab)}
  case (veTy, vtTy) of
        (TInt, TInt) -> do
                      doTy <- typeCheck newEnv d
                      case doTy of
                          TUnit -> return TUnit
                          _ -> newErr pos "For loop body cannot have value"
        _ -> newErr pos "for var type not int"
typeCheck _ (AST.Break _) = return TUnit
typeCheck tab (AST.Let { AST.letDecls=decls
                       , AST.letBody=body }) = do
  newTab <- buildTable decls tab
  (typeCheckList $! newTab) body

-- | Build SymTables from Decls
buildTable :: [AST.Decl] -> SymTables -> IO SymTables
buildTable decls tabs = addDecls tabs decls

