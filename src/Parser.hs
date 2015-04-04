import Control.Monad
import Text.ParserCombinators.Parsec
    
lang = "tiger"
       
type Id = String
type Comment = String
    
data Type = Type Id                 -- Simple Type
          | RecordType [(Id, Type)] -- Record Type (Field, Type)
          | ArrayType Type
            deriving (Show, Eq)

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

parseTypeId :: Parser Type
parseTypeId = liftM Type parseId 

parseTypeField :: Parser (Id, Type)
parseTypeField = do
  ident <- parseId
  spaces >> char ':' >> spaces
  typeId <- parseTypeId
  spaces
  return (ident, typeId)

parseTypeRecord :: Parser Type
parseTypeRecord = do
  char '{' >> spaces
  record <- parseTypeField `sepBy` (char ',' >> spaces)
  char '}'
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
