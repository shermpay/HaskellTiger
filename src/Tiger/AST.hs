module Tiger.AST where

import Data.List as List
import Text.Parsec.Pos (SourcePos)

type Id = String

---------------
-- Operators --
---------------
data Op = Add
        | Sub
        | Mult
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
          | TypeDecl Type Type
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

type Prog = Expr

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
    showData (Nil _) = wrapParens "Nil"
    showData (IntConst _ x) = nodeStr ["IntConst", (show x)]
    showData (StringLit _ x) = nodeStr ["StringLit", (show x)]
    showData (SeqExpr _ e) = nodeStr ["SeqExpr", (showListData e)]
    showData (Call _ f a) = nodeStr ["Call", (show f), (showListData a)]
    showData (InfixOp _ op e1 e2) = nodeStr
                                  ["InfixOp", (show op), (showData e1), (showData e2)]
    showData (NewArr {arrayType=t, arraySize=s, arrayInit=i}) =
        nodeStr ["NewArr", (show t), (showData s), (showData i)]
    showData e = wrapParens $ show e

class (Show a) => ASTNode a where
    showData :: a -> String
    -- sourcePos :: a -> SourcePos
