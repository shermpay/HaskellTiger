module Tiger.TypeChecker where

import qualified Data.List as List
import qualified Data.IORef as IORef
import qualified Control.Monad as Monad
import Text.Parsec.Pos (SourcePos)

import qualified Tiger.AST as AST
import qualified Tiger.Symbol as Sym
import Tiger.Type ((=::))
import qualified Tiger.Type as Ty 

data SymTables = SymTables { valEnv :: Sym.Table Ty.Type
                           , typEnv :: Sym.Table Ty.Type } deriving (Show)

-- | Add a AST.Decl to SymTables
checkDecl :: SymTables -> AST.Decl -> IO SymTables
checkDecl tab@(SymTables { valEnv=ve
                         , typEnv=t })
            (AST.VarDecl
                    { AST.varName=vName
                    , AST.varType=mAnnType
                    , AST.varExpr=expr
                    , AST.varPos=pos }) = do
              exTy <- checkExpr tab expr
              let checkAnn annNode = if exTy =:: vt
                                     then return $ Sym.addString vName exTy ve
                                     else putErr pos
                                              ("Expr and ann type mismatch" ++
                                               "\nExpr: " ++ show vt ++
                                               "\nAnn: " ++ show exTy)
                                              ve
                                         where vt = Sym.getString annNode t
              newValEnv <- case mAnnType of
                             Just annType -> checkAnn annType
                             Nothing -> case exTy of
                                          Ty.TNil -> putErr pos
                                                     "Nil expression needs to be annotated"
                                                     (Sym.addString vName exTy ve)
                                          _ -> return $ Sym.addString vName exTy ve
              return $ SymTables { valEnv=newValEnv, typEnv=t }
            
checkDecl tab@(SymTables { valEnv=t
                       , typEnv=te })
            (AST.TypeDecl pos ident tyNode) = do
              ref <- IORef.newIORef Nothing
              let tempTe = Sym.addString ident (Ty.TName (Sym.Sym ident) ref) te
              ty <- Ty.typeFromAST tempTe tyNode >>= Ty.resolveType tempTe
              IORef.writeIORef ref (Just ty)
              -- myty <- IORef.readIORef ref
              -- putStrLn $ show myty
              -- putStrLn $ show tempTe
              return $SymTables { valEnv=t
                                , typEnv=
                                    case existing of
                                      Just _ -> newErr pos ("Redeclaring type: " ++ ident)
                                      Nothing -> Sym.addString ident ty te }
      where existing = Sym.lookupString ident te

checkDecl tab@(SymTables { valEnv=ve
                       , typEnv=te })
            (AST.FunctionDecl pos funcTy body)
    -- TODO: Check if function is redeclaration
    = case funcTy of
           AST.FuncType ident paramTy retTyId -> do
                    bodyTy <- checkExpr env body
                    newValEnv <- addFunc ident fTy
                    if retTy =:: bodyTy
                    then return fTy
                    else putErr pos "Function body return type mismatch" fTy
                    return $
                           SymTables { valEnv=newValEnv, typEnv=te }
                   where vEnv = newEnv paramTy
                         env = SymTables { valEnv=Sym.addString ident fTy vEnv, typEnv=te }
                         retTy = Sym.getString retTyId te
                         fTy = Ty.TFunc (createFormals vEnv paramTy) (Just retTy)
           AST.ProcType ident paramTy -> do
                    bodyTy <- checkExpr env body
                    funTy <- if Ty.TUnit =:: bodyTy
                             then return fTy
                             -- TFail or fTy testcase40
                             else putErr pos "Procedure body cannot have value" fTy
                    newValEnv <- addFunc ident funTy
                    return $
                           SymTables { valEnv=newValEnv
                                     , typEnv=te }
                   where vEnv = newEnv paramTy
                         env = SymTables { valEnv=Sym.addString ident fTy vEnv, typEnv=te }
                         fTy = Ty.TFunc (createFormals vEnv paramTy) Nothing
      where addIdType env (idt, tyId) = case (Sym.lookupString idt env) of
                            Just _ -> newErr pos ("Invalid list of formals. " ++ idt)
                            Nothing -> Sym.addString idt (Sym.getString tyId te) env
            newEnv paramTy = Sym.mergeTable (List.foldl' addIdType Sym.emptyTable paramTy)
                             ve
            createFormals env = map
                                (\(f, _) -> case Sym.lookupString f env of
                                              Just t -> t
                                              Nothing -> newErr pos "Type does not exist")
            addFunc idt func = case Sym.lookupString idt ve of
                                 Just _ -> putErr pos ("Redeclaring function: " ++ idt) ve
                                 Nothing -> return $ Sym.addString idt func ve

-- | Add a list of Decls to SymTables
checkDecls :: SymTables -> [AST.Decl] -> IO SymTables
checkDecls = Monad.foldM checkDecl

-- | Analyze and build SymTables given a Prog
analyze :: AST.Prog -> SymTables -> IO Ty.Type
analyze (AST.Prog prog) tabs = checkExpr tabs prog 

newErr :: SourcePos -> String -> a
newErr p s = error $ (show p) ++ ": " ++ s      

putErr :: SourcePos -> String -> a -> IO a
putErr p s v = (putStrLn $ (show p) ++ ": " ++ s) >> return v

checkExprList :: SymTables -> [AST.Expr] -> IO Ty.Type
checkExprList tab es = case es of
                         [] -> return Ty.TUnit
                         _ -> mapM (checkExpr tab) es >>= return . last

checkExpr :: SymTables -> AST.Expr -> IO Ty.Type
checkExpr SymTables  { valEnv=vt
                     , typEnv=tt } (AST.IdExpr pos idt)
    = case Sym.lookupString idt vt of
        Just t -> return t
        Nothing -> putErr pos ("Cannot resolve type for " ++ show idt) Ty.TFail
checkExpr tab (AST.FieldDeref pos re (AST.IdExpr _ field)) = do
  recTy <- checkExpr tab re
  case recTy of
    -- TODO: Assuming same order now
    Ty.TRecord fields _ -> case List.lookup (Sym.Sym field) fields of
                      Just (Ty.TName _ ref) -> do
                        maybeTy <- IORef.readIORef ref
                        case maybeTy of
                          Just ty -> return ty
                          Nothing -> putErr pos "record type invalid" Ty.TFail
                      Just ty -> return ty
                      Nothing -> newErr pos "record does not have field"
    _ -> newErr pos "Deref requires record type"
                                                        
checkExpr tab@(SymTables{typEnv=t}) (AST.ArraySub pos arr e) = do
  arrTy <- checkExpr tab arr
  sizeTy <- checkExpr tab e
  case arrTy of
    Ty.TArray ty _ -> if  sizeTy =:: Ty.TInt
                   then return ty
                   else newErr pos "Array subscript not int"
    _ -> newErr pos "Subscript requires array type"
checkExpr _ (AST.Nil _) = return Ty.TNil
checkExpr _ (AST.IntConst _ _) = return Ty.TInt
checkExpr _ (AST.StringLit _ _) = return Ty.TString
checkExpr tab (AST.SeqExpr _ es) = do
  checkExprList tab es
checkExpr tab (AST.Neg pos e) = do
  expTy <- checkExpr tab e
  case expTy of
    Ty.TInt -> return Ty.TInt
    _ -> newErr pos "Cannot negate non-number"
checkExpr tab@(SymTables { valEnv=vTab
                         , typEnv=tTab }) (AST.Call pos f args) = do
  argTy <- mapM (checkExpr tab) args
  if paramTy `Ty.tyListEq` argTy
  then return retTy
  else newErr pos ("argument types do not match" ++ "\nformals: " ++ show paramTy ++ 
           "\nargs:    " ++ show argTy)
  where retTy = case ret of
                  Just x -> x
                  Nothing -> Ty.TUnit
        Ty.TFunc paramTy ret = case Sym.lookupString f vTab of
                              Just t -> t
                              Nothing ->
                                  newErr pos ("Cannot resolve func type " ++ show f)
checkExpr tab (AST.InfixOp pos op l r) = do
  lty <- checkExpr tab l
  rty <- checkExpr tab r
  case op of
      AST.Eq -> check lty rty
      AST.NotEq -> check lty rty
      _ -> case (lty, rty) of
             (Ty.TInt, Ty.TInt) -> return Ty.TInt
             _ -> newErr pos "Cannot apply binary op to non-number"
    where check t1 t2 = if t1 =:: t2
                        then return Ty.TInt
                        else newErr pos "Cannot compare two different types"
checkExpr tab AST.NewArr { AST.arrayType=(AST.Type idt)
                         , AST.arraySize=arrSize
                         , AST.arrayInit=arrInit
                         , AST.arrayPos=pos } = do
  initTy <- checkExpr tab arrInit
  arrSizeTy <- checkExpr tab arrSize
  case arrSizeTy of
    Ty.TInt -> if arrTy =:: initTy
            then return ty
            else newErr pos ("Array initialization expression type mismatch" ++
                             "\nexpected: " ++ show arrTy ++
                             "\nactual:   " ++ show initTy)
      where ty@(Ty.TArray arrTy _) = Sym.getString idt (typEnv tab)
    _ -> newErr pos "Array size has to be integer"
checkExpr tab@(SymTables { valEnv=_
                         , typEnv=tTab })
              (AST.NewRec pos (AST.Type idt) fields) = do
    -- TODO: Currently assumes same ordering
                actTys <- mapM liftTy fields
                let actTypes = map (\(_, t) -> t) actTys
                if expTypes `Ty.tyListEq` actTypes
                then return ty
                else newErr pos ("Record initialization type mismatch" ++
                                 "\nexpected: " ++ (show expTys) ++
                                 "\nactual:   " ++ (show actTys) ++
                                 "\nSymbols: " ++ show tab)
          where (ty, expTys) = case Sym.getString idt tTab of
                           -- (Ty.TRecord ty) -> Ty.TRecord $ map expandOnce ty
                                 recTy@(Ty.TRecord tys _) -> (recTy, tys)
                liftTy (s, expr) = checkExpr tab expr >>= (\x -> return (Sym.Sym s, x))
                expTypes = map (\(_, t) -> case t of
                                             Ty.TName (Sym.Sym s) _ -> Sym.getString s tTab
                                             _ -> t)
                           expTys
checkExpr tab@(SymTables { typEnv=tTab })
               (AST.Assign pos le re) = do
                 lTy <- checkExpr tab le
                 rTy <- checkExpr tab re
                 if lTy =:: rTy
                 then return Ty.TUnit
                 else newErr pos "Assignment type mismatch"
checkExpr tab@(SymTables { valEnv=_
                         , typEnv=tTab })
              (AST.If { AST.ifTest=t
                      , AST.thenExpr=th
                      , AST.elseExpr=el
                      , AST.ifPos=pos}) = do
                ifTy <- checkExpr tab t
                thTy <- checkExpr tab th
                case ifTy of
                  Ty.TInt -> case el of
                            Just e -> do 
                                    elTy <- checkExpr tab e
                                    if elTy =:: thTy
                                    then return elTy
                                    else newErr pos "then else branch types mismatch"
                            Nothing -> case thTy of
                                         Ty.TUnit -> return Ty.TUnit
                                         _ -> newErr pos "if-then cannot have value"
                  _ -> newErr pos "if test type not int"
checkExpr tab AST.While { AST.whileTest=t
                        , AST.whileBody=b
                        , AST.whilePos=pos } = do
  testTy <- checkExpr tab t
  bodyTy <- checkExpr tab b
  case (testTy, bodyTy) of
        (Ty.TInt, Ty.TUnit) -> return Ty.TUnit
        (_, Ty.TUnit) -> newErr pos "while test type not int"
        (Ty.TInt, _) -> newErr pos "while body cannot have value"
        (_, _) -> newErr pos "while test type not int and body cannot have value"
checkExpr tab AST.For { AST.forVarName=vn
                      , AST.forVarExpr=ve
                      , AST.toExpr=vt
                      , AST.doExpr=d
                      , AST.forPos=pos } = do
  veTy <- checkExpr tab ve
  vtTy <- checkExpr tab vt
  let newEnv = SymTables{valEnv=Sym.addString vn veTy (valEnv tab)
                        ,typEnv=(typEnv tab)}
  case (veTy, vtTy) of
        (Ty.TInt, Ty.TInt) -> do
                      doTy <- checkExpr newEnv d
                      case doTy of
                          Ty.TUnit -> return Ty.TUnit
                          _ -> newErr pos "For loop body cannot have value"
        _ -> newErr pos "for var type not int"
checkExpr _ (AST.Break _) = return Ty.TUnit
checkExpr tab (AST.Let { AST.letDecls=decls
                       , AST.letBody=body }) = do
  newTab <- buildTable decls tab
  (checkExprList $! newTab) body

-- | Build SymTables from Decls
buildTable :: [AST.Decl] -> SymTables -> IO SymTables
buildTable decls tabs = checkDecls tabs decls

