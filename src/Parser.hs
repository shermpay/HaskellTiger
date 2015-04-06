import Control.Monad
import Control.Applicative ((<$>))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
-- import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as Tok

def = Tok.LanguageDef { Tok.commentStart = "/*"
                      , Tok.commentEnd = "*/"
                      , Tok.commentLine = ""
                      , Tok.nestedComments = True
                      , Tok.identStart = letter :: Parser Char
                      , Tok.identLetter = (alphaNum <|> char '_')
                      , Tok.opStart = oneOf ":=.-+-*/<>&|"
                      , Tok.opLetter = oneOf ":=.-+-*/<>&|"
                      , Tok.reservedNames = 
                          ["type", "array", "of", "function", "var", "nil", "break"
                          , "if", "then", "else", "while", "for", "to", "do"
                          , "let", "in", "end"]
                      , Tok.reservedOpNames = [":", ":=", ".", "-"
                                              , "+", "-", "*", "/"
                                              , "=", "<>", ">", "<", "<=", ">="
                                              , "&", "|"]
                      , Tok.caseSensitive = True}
           
lexer@Tok.TokenParser { Tok.identifier = identifier
                      , Tok.reserved = reserved
                      , Tok.operator = operator
                      , Tok.reservedOp = reservedOp
                      , Tok.charLiteral = charLiteral
                      , Tok.stringLiteral = stringLiteral
                      , Tok.symbol = symbol
                      , Tok.lexeme = lexeme
                      , Tok.whiteSpace = whiteSpace
                      , Tok.parens = parens 
                      , Tok.braces = braces
                      , Tok.brackets = brackets
                      , Tok.comma = comma
                      , Tok.dot = dot
                      , Tok.colon = colon
                      , Tok.commaSep = commaSep
                      , Tok.commaSep1 = commaSep1 } = Tok.makeTokenParser def
             
lang = "tiger"
       
type Id = String
type Comment = String
    
readExpr :: String -> String
readExpr input = 
    case parse identifier lang input of
      Left err -> show err
      Right val -> val
                   
data Decl = VarDecl {varName :: Id
                     , varType :: Maybe Type
                     , varExpr :: Expr}
          | FunctionDecl FunctionType Expr
          | TypeDecl Type
          deriving Show
-----------------------
-- Type Declarations --
-----------------------
type FieldType = (Id, Type)
type RecordType = [FieldType]
data Type = Type Id                 -- Simple Type
          | RecordType RecordType -- Record Type (Field, Type)
          | ArrayType Type
            deriving (Show, Eq)

parseTypeId :: Parser Type
parseTypeId = liftM Type identifier 

parseTypeField :: Parser FieldType
parseTypeField = do
  ident <- identifier
  reservedOp ":"
  typeId <- parseTypeId
  return (ident, typeId)

parseTypeFields :: Parser RecordType
parseTypeFields = commaSep parseTypeField 

parseTypeRecord :: Parser Type
parseTypeRecord = do
  record <- braces parseTypeFields 
  return $ RecordType record

parseTypeArray :: Parser Type
parseTypeArray = do
  reserved "array of"
  typeId <- parseTypeId
  return $ ArrayType typeId

parseTypeVal :: Parser Type
parseTypeVal = parseTypeArray
    <|> parseTypeId
    <|> parseTypeRecord

parseTypeDecl :: Parser FieldType 
parseTypeDecl = do
  reserved "type"
  ident <- identifier 
  reservedOp "="
  typeVal <- parseTypeVal
  return $ (ident, typeVal)

readTypeDecl :: String -> String
readTypeDecl input =
    case parse parseTypeDecl lang input of
      Left err -> show err
      Right val -> show val

---------------------------
-- Variable Declarations --
---------------------------
parseTypeAnn :: Parser Type
parseTypeAnn = do
  reservedOp ":"
  typeId <- parseTypeId
  return typeId

parseVarDecl :: Parser Decl
parseVarDecl = do
  reserved "var"
  ident <- identifier
  varT <- optionMaybe $ try parseTypeAnn
  reservedOp ":="
  expr <- parseExpr
  return VarDecl {varName=ident, varType=varT, varExpr=expr}

readVarDecl :: String -> String
readVarDecl input =
    case parse parseVarDecl lang input of
      Left err -> show err
      Right val -> show val

-------------------------------------
-- Function/Procedure Declarations --
-------------------------------------
data FunctionType = FuncType Id RecordType Type
                  | ProcType Id RecordType
                    deriving (Show, Eq)

parseFunctionDecl :: Parser Decl
parseFunctionDecl = do
  reserved "function"
  ident <- identifier
  paramDecl <- parens parseTypeFields
  retType <- optionMaybe $ try parseTypeAnn
  reservedOp "="
  body <- parseExpr
  return $ FunctionDecl (case retType of
                           Nothing -> ProcType ident paramDecl
                           Just ret -> FuncType ident paramDecl ret) body

readFunctionDecl :: String -> String
readFunctionDecl input =
    case parse parseFunctionDecl lang input of
      Left err -> show err
      Right val -> show val
                   
--------------
-- L-Values --
--------------
{-
  lvalue -> id
         -> lvalue . id
         -> lvalue [ exp ]
  
  Left Recursion:
  lvalue -> id deref
  deref  -> . id deref
         -> [ exp ] deref
         -> epsilon
-}
data LValue = Var Id
            | FieldDeref LValue LValue -- (Record, Field)
            | ArraySub LValue Expr
              deriving (Show)

parseLValue :: Parser LValue
parseLValue =
    parseVar `chainl1` parseFieldDeref

parseFieldDeref :: Parser (LValue -> LValue -> LValue)
parseFieldDeref = do
  dot
  return FieldDeref
         
parseVar :: Parser LValue
parseVar = liftM Var identifier

readLValue :: String -> String
readLValue input =
    case parse parseLValue lang input of
      Left err -> show err
      Right val -> show val
  
-----------------
-- Expressions --
-----------------
data Op = Plus
        | Minus
        | Times
        | Div
        | Eq
        | NE
        | LT
        | LTE
        | GT
        | GTE
        | And
        | Or
        deriving Show

data Expr = LValExpr LValue
          | Nil
          | IntLit Int
          | StringLit String
          | SeqExpr [Expr]
          | Neg Expr
          | Call [Expr]
          | Infix Expr Op Expr
          | NewArr Type Expr Expr
          | NewRec Type Expr
          | NewField Id Expr
          | Assign LValue Expr
          | If { ifExpr :: Expr, thenExpr :: Expr,  elseExpr :: Expr }
          | While Expr  Expr
          | For { forVarName :: Id, forVarExpr :: Expr, toExpr :: Expr, doExpr :: Expr }
          deriving Show

parseExpr :: Parser Expr
parseExpr = liftM (LValExpr . Var) identifier
