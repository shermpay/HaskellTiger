import Control.Monad
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec
    
lang = "tiger"
       
type Id = String
type Comment = String
    
parseId :: Parser Id
parseId = do
  first <- letter
  rest <- many (alphaNum <|> char '_')
  return (first:rest)

parseComment :: Parser Comment
parseComment = do
  string "/*" 
  manyTill anyChar (try $ string "*/")

readExpr :: String -> String
readExpr input = 
    case parse parseId lang input of
      Left err -> show err
      Right val -> val

readComment :: String -> String
readComment input = 
    case parse parseComment lang input of
      Left err -> show err
      Right val -> val
                   
-----------------------
-- Type Declarations --
-----------------------
data Type = Type Id                 -- Simple Type
          | RecordType [(Id, Type)] -- Record Type (Field, Type)
          | ArrayType Type
            deriving (Show, Eq)

parseTypeId :: Parser Type
parseTypeId = liftM Type parseId 

parseTypeField :: Parser (Id, Type)
parseTypeField = do
  ident <- parseId
  spaces >> char ':' >> spaces
  typeId <- parseTypeId
  spaces
  return (ident, typeId)

parseTypeFields :: Parser [(Id, Type)]
parseTypeFields = parseTypeField `sepBy` (char ',' >> spaces)

parseTypeRecord :: Parser Type
parseTypeRecord = do
  record <- between (char '{' >> spaces) (char '}') parseTypeFields 
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

parseTypeDecl :: Parser (Id, Type) 
parseTypeDecl = do
  string "type" >> spaces
  ident <- parseId 
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
  ident <- parseId
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
data FunctionType = FuncType ([(Id, Type)] , Type)
                  | ProcType [(Id, Type)]
                    deriving (Show, Eq)

type FunctionDecl = (FunctionType, Expr)
                    
parseFunctionDecl :: Parser FunctionDecl
parseFunctionDecl = do
  string "function" >> spaces
  ident <- parseId
  paramDecl <- between (spaces >> char '(' >> spaces) (spaces >> char ')' >> spaces) 
               parseTypeFields
  retType <- optionMaybe $ try parseTypeAnn
  spaces >> (char '=') >> spaces
  body <- parseExpr
  return $ (case retType of
              Nothing -> ProcType paramDecl
              Just ret -> FuncType (paramDecl, ret)
           , body)

readFunctionDecl :: String -> String
readFunctionDecl input =
    case parse parseFunctionDecl lang input of
      Left err -> show err
      Right val -> show val

-----------------
-- Expressions --
-----------------
data Expr = Var Id
          deriving Show
parseExpr :: Parser Expr
parseExpr = liftM Var parseId
