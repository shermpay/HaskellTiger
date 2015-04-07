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
                      , Tok.natural = natural
                      , Tok.integer = integer
                      , Tok.symbol = symbol
                      , Tok.lexeme = lexeme
                      , Tok.whiteSpace = whiteSpace
                      , Tok.parens = parens 
                      , Tok.braces = braces
                      , Tok.brackets = brackets
                      , Tok.comma = comma
                      , Tok.dot = dot
                      , Tok.colon = colon
                      , Tok.semiSep = semiSep
                      , Tok.commaSep = commaSep
                      , Tok.commaSep1 = commaSep1 } = Tok.makeTokenParser def
             
lang = "tiger"
       
type Id = String
type Comment = String
    
data Decl = VarDecl { varName :: Id
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
parseLValue :: Parser Expr
parseLValue = (try parseArraySub <|> parseIdExpr) `chainl1` parseFieldDeref

parseFieldDeref :: Parser (Expr -> Expr -> Expr)
parseFieldDeref = do
  dot
  return FieldDeref

parseArraySub :: Parser Expr
parseArraySub = do
  var <- parseIdExpr
  expr <- brackets parseExpr
  notFollowedBy $ reserved "of"
  return $ ArraySub var expr
         
parseIdExpr :: Parser Expr
parseIdExpr = liftM IdExpr identifier

readLValue :: String -> String
readLValue input =
    case parse parseLValue lang input of
      Left err -> show err
      Right val -> show val
  
-----------------
-- Expressions --
-----------------
data Op = Add
        | Sub
        | Mult
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

data Expr = IdExpr Id
          | FieldDeref Expr Expr -- (Record, Field)
          | ArraySub Expr Expr
          | Nil
          | IntConst Integer
          | StringLit String
          | SeqExpr [Expr]
          | Neg Expr
          | Call Expr [Expr]
          | InfixOp Op Expr Expr
          | NewArr {arrayType :: Type,  arraySize :: Expr, arrayInit ::  Expr}
          | NewRec Type [FieldInit]
          | Assign Expr Expr
          | If { ifExpr :: Expr, thenExpr :: Expr,  elseExpr :: Expr }
          | IfThen Expr Expr
          | While Expr Expr
          | For { forVarName :: Id, forVarExpr :: Expr, toExpr :: Expr, doExpr :: Expr }
          | Break
          | Let [Decl] [Expr]
          deriving Show
                   
type FieldInit = (Id, Expr)

parseIdAndExpr :: Parser Expr
parseIdAndExpr = (try parseArraySub 
                 <|> (try parseCall) 
                 <|> (try parseNewArr) 
                 <|> (try parseNewRec)
                 <|> (try parseAssign)
                 <|> parseIdExpr) 
                 `chainl1` parseFieldDeref

parseSeqExpr :: Parser Expr
parseSeqExpr = do { exprs <- parens $ semiSep parseExpr; return $ SeqExpr exprs }

parseNeg :: Parser Expr
parseNeg = do { reservedOp "-"; expr <- parseExpr ; return $ Neg expr }

parseCall :: Parser Expr
parseCall = do
  funcName <- parseIdExpr
  args <- parens $ commaSep parseExpr
  return $ Call funcName args

parseNewArr :: Parser Expr
parseNewArr = do
  arrType <- parseTypeId
  size <- brackets parseExpr
  reserved "of"
  initVal <- parseExpr
  return $ NewArr {arrayType=arrType, arraySize=size, arrayInit=initVal}

parseFieldInit :: Parser (Id, Expr)
parseFieldInit = do
    ident <- identifier
    reservedOp "="
    expr <- parseExpr
    return (ident, expr)
        
parseNewRec :: Parser Expr
parseNewRec = do
  recType <- parseTypeId
  fieldInits <- braces $ commaSep  parseFieldInit
  return $ NewRec recType fieldInits

parseAssign :: Parser Expr
parseAssign = do
  var <- parseIdExpr
  reservedOp ":="
  expr <- parseExpr
  return $ Assign var expr

parseIf :: Parser Expr
parseIf = do
  reserved "if"
  ifExpr <- parseExpr
  reserved "then"
  thenExpr <- parseExpr
  maybeElse <- optionMaybe $ do { reserved "else";
                                  elseExpr <- parseExpr;
                                  return elseExpr }
  case maybeElse of
    Nothing -> return $ IfThen ifExpr thenExpr
    Just elseExpr -> return $ If { ifExpr=ifExpr, thenExpr=thenExpr, elseExpr=elseExpr }

operators = [ [Infix  (reservedOp "*"   >> return (InfixOp Div )) AssocLeft]
            , [Infix  (reservedOp "/"   >> return (InfixOp Mult)) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (InfixOp Add )) AssocLeft]
            , [Infix  (reservedOp "-"   >> return (InfixOp Sub )) AssocLeft]
            ]
parseExpr :: Parser Expr
parseExpr = buildExpressionParser operators parseGenExpr

parseGenExpr :: Parser Expr
parseGenExpr = parseIdAndExpr 
            -- Parses LValues and Funcalls and New Array
            <|> do { (reserved "nil"); return Nil }
            <|> do { val <- natural; return $ IntConst val }
            <|> do { val <- stringLiteral; return $ StringLit val }
            <|> parseSeqExpr
            <|> parseNeg
            <|> parseIf

readExpr :: String -> String
readExpr input = 
    case parse parseExpr lang input of
      Left err -> show err
      Right val -> show val
