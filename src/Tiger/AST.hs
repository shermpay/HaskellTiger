{-# LANGUAGE FlexibleInstances #-}
module Tiger.AST
    ( Id
    , Prog (Prog)
    , Op (..)
    , Type (..)
    , FunctionType (..)
    , FieldType
    , RecordType
    , Decl (..)
    , Expr (..)
    , showData
    , showAST ) where

import Data.List as List
import Text.Parsec.Pos (SourcePos, newPos, initialPos)

type Id = String

---------------
-- Operators --
---------------
data Op = Add
        | Sub
        | Mul
        | Div
        | Eq
        | NE
        | Less
        | LessEq
        | Greater
        | GreaterEq
        | And
        | Or
        deriving (Show, Ord, Eq)

showOp :: Op -> String
showOp Add       = "+"
showOp Sub       = "-"
showOp Mul       = "*"
showOp Div       = "/"
showOp Eq        = "="
showOp NE        = "<>"
showOp Less      = "<"
showOp LessEq    = "<="
showOp Greater   = ">"
showOp GreaterEq = ">="
showOp And       = "&"
showOp Or        = "|"
-----------------------
-- Type Declarations --
-----------------------
type FieldType = (Id, Type)
type RecordType = [FieldType]
data Type = Type Id                 -- Simple Type
          | RecordType RecordType -- Record Type (Field, Type)
          | ArrayType Type
            deriving (Show, Eq)

-------------------------------------
-- Function/Procedure Declarations --
-------------------------------------
data FunctionType = FuncType Id RecordType Type
                  | ProcType Id RecordType
                    deriving (Show, Eq)

------------------
-- Declarations --
------------------
data Decl = VarDecl { varName :: Id
                    , varType :: Maybe Type
                    , varExpr :: Expr}
          | FunctionDecl FunctionType Expr
          | TypeDecl Id Type
          deriving Show


-----------------
-- Expressions --
-----------------
type FieldInit = (Id, Expr)

data Expr = IdExpr SourcePos Id
          | FieldDeref SourcePos Expr Expr -- (Record, Field)
          | ArraySub SourcePos Expr Expr
          | Nil SourcePos 
          | IntConst SourcePos Integer
          | StringLit SourcePos String
          | SeqExpr SourcePos [Expr]
          | Neg SourcePos Expr
          | Call SourcePos Expr [Expr]
          | InfixOp SourcePos Op Expr Expr
          | NewArr {arrayType :: Type,  arraySize :: Expr, arrayInit ::  Expr
                   , arrayPos :: SourcePos }
          | NewRec SourcePos Type [FieldInit]
          | Assign SourcePos Expr Expr
          | If { ifTest :: Expr, thenExpr :: Expr,  elseExpr :: Maybe Expr
               , ifPos :: SourcePos }
          | While{ whileTest :: Expr, whileBody :: Expr, whilePos :: SourcePos }
          | For { forVarName :: Id, forVarExpr :: Expr, toExpr :: Expr, doExpr :: Expr 
                , forPos :: SourcePos }
          | Break SourcePos
          | Let { letDecls :: [Decl], letBody ::  [Expr], letPos :: SourcePos }
          deriving Show

newtype Prog = Prog Expr

----------------
-- Output AST --
----------------
showListData :: [Expr] -> String
showListData lst =
    '[' : (concat $ List.intersperse ", " $ map showData lst) ++ "]"

wrapIn :: String -> String -> String -> String
wrapIn l r str = l ++ str ++ r
wrapParens :: String -> String
wrapParens = wrapIn "(" ")"

spacedConcat :: [String] -> String
spacedConcat = concat . List.intersperse " "

nodeStr :: [String] -> String
nodeStr = wrapParens . spacedConcat

instance ASTNode Expr where
    showData (IdExpr _ i) = nodeStr ["IdExpr", i]
    showData (Nil _) = wrapParens "Nil"
    showData (IntConst _ x) = nodeStr ["IntConst", (show x)]
    showData (StringLit _ x) = nodeStr ["StringLit", (show x)]
    showData (SeqExpr _ e) = nodeStr ["SeqExpr", (showListData e)]
    showData (Call _ f a) = nodeStr ["Call", (show f), (showListData a)]
    showData (InfixOp _ op e1 e2) = nodeStr
                                  ["InfixOp", (show op), (showData e1), (showData e2)]
    showData (NewArr {arrayType=t, arraySize=s, arrayInit=i}) =
        nodeStr ["NewArr", (show t), "Size", (showData s), "Init", (showData i)]
    showData (NewRec _ ty fieldInits) = 
        nodeStr (["NewRec", (showData ty)] ++
                 map (\(i, e) -> i ++ " : " ++ (showData e)) fieldInits)
    showData (If test thExpr elExpr _) =
        nodeStr (["If",  showData test
                 , showData thExpr
                 , case elExpr of
                     Just expr -> showData expr
                     Nothing -> ""])
    showData e = wrapParens $ show e

-- This requires flexible instances
instance ASTNode FieldType where
    showData (ident, ty) = ident ++ " : " ++ (showData ty)

instance ASTNode Type where
    showData (Type ty) = ty
    showData (RecordType rectype) = concat $ map showData rectype 

class (Show a) => ASTNode a where
    showData :: a -> String
    -- sourcePos :: a -> SourcePos

showAST :: Expr -> String
showAST node = 
    showExpr 0 node 

makeIndent :: Int -> String -> String
makeIndent n str = List.foldl' (\l x -> x:l) str (take n $ repeat ' ')

inc :: Int -> Int
inc = (+ 2)

dinc :: Int -> Int
dinc = inc . inc

tinc :: Int -> Int
tinc = inc . inc . inc

showExprs :: Int -> [Expr] -> String
showExprs indent lst = concatMap ((++ "\n") . showExpr indent) lst

showExpr :: Int -> Expr -> String
showExpr indent (FieldDeref _ obj field) = 
    makeIndent indent "FIELD_DEREF\n" ++
    showExpr (inc indent) obj ++ "\n" ++
    makeIndent (inc indent) "DOT\n" ++
    showExpr (inc indent) field
showExpr indent (ArraySub _ arr sub) = 
    makeIndent indent "ARR_SUB\n" ++
    showExpr (inc indent) arr ++ "\n" ++
    makeIndent (inc indent) "SUB\n" ++
    showExpr (inc indent) sub
showExpr indent (SeqExpr _ e) = makeIndent indent
                                "SEQEXPR\n" ++ showExprs (inc indent) e
showExpr indent (Call _ f a) = makeIndent indent
                               "CALL\n" ++ showExpr (inc indent) f ++ "\n" ++
                               makeIndent (inc indent)
                               "ARGS\n" ++ showExprs (dinc indent) a
showExpr indent (InfixOp _ op e1 e2) = 
    showExpr indent e1 ++ "\n" ++ makeIndent indent (showOp op) ++ "\n" ++ 
    showExpr indent e2
    -- makeIndent indent (show op) ++
    --                               ('\n':showExpr (inc indent) e1) ++
    --                               ('\n':showExpr (inc indent) e2)
showExpr indent (If test te ee _) = makeIndent indent ("IF\n" ++
                                    showExpr (inc indent) test) ++ "\n" ++

                                    makeIndent indent ("THEN\n" ++
                                    showExpr (inc indent) te) ++ "\n" ++

                                    makeIndent indent
                                    "ELSE\n" ++
                                    (case ee of
                                       Just e -> showExpr (inc indent) e
                                       Nothing -> "")
showExpr indent (Let decls body _)  = makeIndent indent
                               "LET\n" ++ showDecls ++ "\nIN\n" ++
                               showExprs (inc indent) body
                         where showDecls = concatMap
                                           ((++ "\n") . (showDecl (inc indent)))
                                           decls
showExpr indent NewArr { arrayType=ty, arraySize=size, arrayInit=expr } =
    makeIndent indent "ARRAY\n" ++ 
    showType (inc indent) ty ++ "\n" ++
    makeIndent (inc indent) "SIZE\n" ++ 
    showExpr (dinc indent) size ++ "\n" ++
    makeIndent (inc indent)  "ARRAYINIT\n" ++ 
    showExpr (dinc indent) expr
showExpr indent (NewRec _ ty fieldInits) =
    showType (inc indent) ty ++ " :\n" ++ 
     (concatMap (\(i, e) -> makeIndent (inc . inc $ indent)
                            i ++ " = " ++ showData e ++ "\n") fieldInits)
showExpr indent (Assign _ lexpr rexpr) = 
    makeIndent indent "ASSIGN\n" ++
    showExpr (inc indent) lexpr ++ " := \n" ++
    showExpr (inc indent) rexpr ++ "\n"
showExpr indent While { whileTest=test, whileBody=body } =
    makeIndent indent "WHILE\n" ++
    makeIndent (inc indent) "TEST\n" ++
    showExpr (dinc indent) test ++ "\n" ++
    makeIndent (inc indent) "WHILE_BODY\n" ++
    showExpr (dinc indent) body
showExpr indent For { forVarName=var, forVarExpr=expr, toExpr=to, doExpr=body } =
    makeIndent indent "FOR\n" ++
    makeIndent (inc indent) "FOR_VAR " ++ var ++ " :=\n" ++
    showExpr (dinc indent) expr ++ "\n" ++
    makeIndent (inc indent) "TO\n" ++
    showExpr (dinc indent) to ++ "\n" ++
    makeIndent (inc indent) "FOR_BODY\n" ++
    showExpr (dinc indent) body
showExpr indent e = makeIndent indent (showData e) 
                    
showType :: Int -> Type -> String
showType indent (Type typeId) = makeIndent indent typeId
showType indent (RecordType ty) = showRecType indent ty
showType indent (ArrayType ty) = showType indent ty 
                                 
showDecl :: Int -> Decl -> String
showDecl indent (VarDecl i ty e) = makeIndent indent
                                   "VAR\n" ++
                                   makeIndent (inc indent)
                                              i ++ maybeTypeStr ++ " :=\n" ++
                                   (showExpr (inc indent) e)
                                   where maybeTypeStr = 
                                             case ty of
                                               Just t -> " : "
                                                         ++ showData t
                                               Nothing -> ""
showDecl indent (FunctionDecl funtype e) = makeIndent indent
                                           "FUNCTION_DECL\n" ++
                                           showFuncType (inc indent) funtype ++ "\n" ++
                                           makeIndent (inc indent)
                                           "FUNC_BODY\n" ++ showExpr (tinc indent) e
showDecl indent (TypeDecl ty1 ty2) = makeIndent indent 
                                     "TYPE\n" ++ makeIndent (inc indent) ty1 ++ " :=\n" ++
                                     showType (dinc indent) ty2

showFuncType :: Int -> FunctionType -> String
showFuncType indent (FuncType funcId rectype rettype) = 
    makeIndent indent
      "FUNCTION " ++ funcId ++ "\n" ++
      makeIndent (inc indent) "PARAMS\n" ++ showRecType (inc indent) rectype ++
                     makeIndent (inc indent)
                                    "RET\n" ++ showType (dinc indent) rettype
showFuncType indent (ProcType procId rectype) =
    makeIndent indent "PROC " ++ procId ++ "\n" ++
    makeIndent (inc indent) "PARAMS\n" ++ showRecType (inc indent) rectype

showRecType :: Int -> RecordType -> String
showRecType indent rectype =
    concatMap (\x -> makeIndent (inc indent) $ showData x ++ "\n") rectype
