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
                      , Tok.opStart = oneOf ":="
                      , Tok.opLetter = oneOf "="
                      , Tok.reservedNames = 
                          ["type", "array", "of", "function", "var", "nil", "break"
                          , "if", "then", "else", "while", "for", "to", "do"
                          , "let", "in", "end"]
                      , Tok.reservedOpNames = [":", ":=", ".", "=", "-"]
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
  spaces >> char ':' >> spaces
  typeId <- parseTypeId
  spaces
  return (ident, typeId)

parseTypeFields :: Parser RecordType
parseTypeFields = parseTypeField `sepBy` (char ',' >> spaces)

parseTypeRecord :: Parser Type
parseTypeRecord = do
  record <- braces parseTypeFields 
  return $ RecordType record

parseTypeArray :: Parser Type
parseTypeArray = do
  string "array of" >> spaces
  typeId <- parseTypeId
  return $ ArrayType typeId

parseTypeVal :: Parser Type
parseTypeVal = parseTypeArray
    <|> parseTypeId
    <|> parseTypeRecord

parseTypeDecl :: Parser FieldType 
parseTypeDecl = do
  string "type" >> spaces
  ident <- identifier 
  spaces >> char '=' >> spaces
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
data VarDecl = VarDecl {varName :: Id
                       , varType :: Maybe Type
                       , varExpr :: Expr} deriving (Show)
             
parseTypeAnn :: Parser Type
parseTypeAnn = do
  char ':' >> spaces
  typeId <- parseTypeId
  spaces
  return typeId

parseVarDecl :: Parser VarDecl
parseVarDecl = do
  string "var" >> spaces
  ident <- identifier
  spaces
  varT <- optionMaybe $ try parseTypeAnn
  (string ":=") >> spaces
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
data FunctionType = FuncType RecordType Type
                  | ProcType RecordType
                    deriving (Show, Eq)

type FunctionDecl = (FunctionType, Expr)
                    
parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = do
  string "function" >> spaces
  ident <- identifier
  paramDecl <- parens parseTypeFields
  retType <- optionMaybe $ try parseTypeAnn
  spaces >> (char '=') >> spaces
  body <- parseExpr
  return $ (case retType of
              Nothing -> ProcType paramDecl
              Just ret -> FuncType paramDecl ret
           , body)

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
data LValue = LVar Id
            | FieldDeref LValue LValue -- (Record, Field)
            | ArraySub LValue Expr
              deriving (Show)

parseLValue :: Parser LValue
parseLValue =
    parseLVar `chainl1` parseFieldDeref

parseFieldDeref :: Parser (LValue -> LValue -> LValue)
parseFieldDeref = do
  spaces >> (char '.') >> spaces
  return FieldDeref
         
parseLVar :: Parser LValue
parseLVar = liftM LVar identifier

readLValue :: String -> String
readLValue input =
    case parse parseLValue lang input of
      Left err -> show err
      Right val -> show val
  
-----------------
-- Expressions --
-----------------
data Expr = Var Id
          deriving Show
parseExpr :: Parser Expr
parseExpr = liftM Var identifier
