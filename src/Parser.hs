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
          | TypeDecl Type Type
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

typeIdParser :: Parser Type
typeIdParser = liftM Type identifier 

typeFieldParser :: Parser FieldType
typeFieldParser = do
  ident <- identifier
  reservedOp ":"
  typeId <- typeIdParser
  return (ident, typeId)

typeFieldsParser :: Parser RecordType
typeFieldsParser = commaSep typeFieldParser 

typeRecordParser :: Parser Type
typeRecordParser = do
  record <- braces typeFieldsParser 
  return $ RecordType record

typeArrayParser :: Parser Type
typeArrayParser = do
  reserved "array of"
  typeId <- typeIdParser
  return $ ArrayType typeId

typeValParser :: Parser Type
typeValParser = typeArrayParser
    <|> typeIdParser
    <|> typeRecordParser

typeDeclParser :: Parser Decl 
typeDeclParser = do
  reserved "type"
  typeId <- typeIdParser
  reservedOp "="
  typeVal <- typeValParser
  return $ TypeDecl typeId typeVal

readTypeDecl :: String -> String
readTypeDecl input =
    case parse typeDeclParser lang input of
      Left err -> show err
      Right val -> show val

---------------------------
-- Variable Declarations --
---------------------------
typeAnnParser :: Parser Type
typeAnnParser = do
  reservedOp ":"
  typeId <- typeIdParser
  return typeId

varDeclParser :: Parser Decl
varDeclParser = do
  reserved "var"
  ident <- identifier
  varT <- optionMaybe $ try typeAnnParser
  reservedOp ":="
  expr <- exprParser
  return VarDecl {varName=ident, varType=varT, varExpr=expr}

readVarDecl :: String -> String
readVarDecl input =
    case parse varDeclParser lang input of
      Left err -> show err
      Right val -> show val

-------------------------------------
-- Function/Procedure Declarations --
-------------------------------------
data FunctionType = FuncType Id RecordType Type
                  | ProcType Id RecordType
                    deriving (Show, Eq)

functionDeclParser :: Parser Decl
functionDeclParser = do
  reserved "function"
  ident <- identifier
  paramDecl <- parens typeFieldsParser
  retType <- optionMaybe $ try typeAnnParser
  reservedOp "="
  body <- exprParser
  return $ FunctionDecl (case retType of
                           Nothing -> ProcType ident paramDecl
                           Just ret -> FuncType ident paramDecl ret) body

readFunctionDecl :: String -> String
readFunctionDecl input =
    case parse functionDeclParser lang input of
      Left err -> show err
      Right val -> show val

declParser :: Parser Decl
declParser = typeDeclParser
            <|> varDeclParser
            <|> functionDeclParser
                   
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
lValueParser :: Parser Expr
lValueParser = (try arraySubParser <|> idExprParser) `chainl1` fieldDerefParser

fieldDerefParser :: Parser (Expr -> Expr -> Expr)
fieldDerefParser = do
  dot
  return FieldDeref

arraySubParser :: Parser Expr
arraySubParser = do
  var <- idExprParser
  expr <- brackets exprParser
  notFollowedBy $ reserved "of"
  return $ ArraySub var expr
         
idExprParser :: Parser Expr
idExprParser = liftM IdExpr identifier

readLValue :: String -> String
readLValue input =
    case parse lValueParser lang input of
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

idAndExprParser :: Parser Expr
idAndExprParser = (try arraySubParser 
                 <|> (try callParser) 
                 <|> (try newArrParser) 
                 <|> (try newRecParser)
                 <|> (try assignParser)
                 <|> idExprParser) 
                 `chainl1` fieldDerefParser

seqExprParser :: Parser Expr
seqExprParser = do { exprs <- parens $ semiSep exprParser; return $ SeqExpr exprs }

negParser :: Parser Expr
negParser = do { reservedOp "-"; expr <- exprParser ; return $ Neg expr }

callParser :: Parser Expr
callParser = do
  funcName <- idExprParser
  args <- parens $ commaSep exprParser
  return $ Call funcName args

newArrParser :: Parser Expr
newArrParser = do
  arrType <- typeIdParser
  size <- brackets exprParser
  reserved "of"
  initVal <- exprParser
  return $ NewArr {arrayType=arrType, arraySize=size, arrayInit=initVal}

fieldInitParser :: Parser (Id, Expr)
fieldInitParser = do
    ident <- identifier
    reservedOp "="
    expr <- exprParser
    return (ident, expr)
        
newRecParser :: Parser Expr
newRecParser = do
  recType <- typeIdParser
  fieldInits <- braces $ commaSep  fieldInitParser
  return $ NewRec recType fieldInits

assignParser :: Parser Expr
assignParser = do
  var <- idExprParser
  reservedOp ":="
  expr <- exprParser
  return $ Assign var expr

ifParser :: Parser Expr
ifParser = do
  reserved "if"
  ifExpr <- exprParser
  reserved "then"
  thenExpr <- exprParser
  maybeElse <- optionMaybe $ do { reserved "else";
                                  elseExpr <- exprParser;
                                  return elseExpr }
  case maybeElse of
    Nothing -> return $ IfThen ifExpr thenExpr
    Just elseExpr -> return $ If { ifExpr=ifExpr, thenExpr=thenExpr, elseExpr=elseExpr }

whileParser :: Parser Expr
whileParser = do
  reserved "while"
  test <- exprParser
  reserved "do"
  body <- exprParser
  return $ While test body
         
forParser :: Parser Expr
forParser = do
  reserved "for"
  ident <- identifier
  reservedOp ":="
  idExpr <- exprParser
  reserved "to"
  toExpr <- exprParser
  reserved "do"
  doExpr <- exprParser
  return $ For { forVarName=ident, forVarExpr=idExpr, toExpr=toExpr, doExpr=doExpr }

letParser :: Parser Expr
letParser = do
  reserved "let"
  decls <- declParser `sepBy` whiteSpace
  reserved "in"
  body <- semiSep exprParser
  reserved "end"
  return $ Let decls body

operators = [ [Infix  (reservedOp "*"   >> return (InfixOp Div )) AssocLeft]
            , [Infix  (reservedOp "/"   >> return (InfixOp Mult)) AssocLeft]
            , [Infix  (reservedOp "+"   >> return (InfixOp Add )) AssocLeft]
            , [Infix  (reservedOp "-"   >> return (InfixOp Sub )) AssocLeft]
            ]
exprParser :: Parser Expr
exprParser = buildExpressionParser operators genExprParser

genExprParser :: Parser Expr
genExprParser = idAndExprParser 
            -- Parses LValues and Funcalls and New Array
            <|> do { (reserved "nil"); return Nil }
            <|> do { val <- natural; return $ IntConst val }
            <|> do { val <- stringLiteral; return $ StringLit val }
            <|> seqExprParser
            <|> negParser
            <|> ifParser
            <|> whileParser
            <|> forParser
            <|> letParser

readExpr :: String -> String
readExpr input = 
    case parse exprParser lang input of
      Left err -> show err
      Right val -> show val
