module Tiger.Parser ( parseProg ) where
  
import qualified Data.Map as Map
import Data.Functor.Identity (Identity)
import Control.Monad (liftM)
import Text.Parsec (letter, alphaNum, char, oneOf, parse, getPosition, optionMaybe, sepBy, try, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (Operator, Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Text.ParserCombinators.Parsec (chainl1, notFollowedBy)
import qualified Text.Parsec.Token as Tok
    
import Tiger.AST

{- Token Definitions -}
def :: Tok.GenLanguageDef String () Identity
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
           
{- Creates a Lexer -}
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
       
-----------------------
-- Type Declarations --
-----------------------
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
  pos <- getPosition
  dot
  return $ FieldDeref pos

arraySubParser :: Parser Expr
arraySubParser = do
  var <- idExprParser
  pos <- getPosition
  expr <- brackets exprParser
  notFollowedBy $ reserved "of"
  return $ ArraySub pos var expr
         
idExprParser :: Parser Expr
idExprParser = do
  pos <- getPosition
  ident <- identifier
  return $ IdExpr pos ident

readLValue :: String -> String
readLValue input =
    case parse lValueParser lang input of
      Left err -> show err
      Right val -> show val
  
-----------------
-- Expressions --
-----------------
operatorMap :: Map.Map Op String
operatorMap = Map.fromList [ (Add, "+")
                           , (Sub, "-")
                           , (Mult, "*")
                           , (Div, "/")
                           , (Eq, "=")
                           , (NE, "<>")
                           , (Less, "<")
                           , (LessEq, "<=")
                           , (Greater, ">")
                           , (GreaterEq, ">=")
                           , (And, "&")
                           , (Or, "|")
                           ]


idAndExprParser :: Parser Expr
idAndExprParser = (try assignParser
                 <|> (try callParser) 
                 <|> (try newArrParser) 
                 <|> (try newRecParser)
                 <|> (try arraySubParser)
                 <|> idExprParser) 
                 `chainl1` fieldDerefParser

seqExprParser :: Parser Expr
seqExprParser = do
  pos <- getPosition
  exprs <- parens $ semiSep exprParser
  return $ SeqExpr pos exprs

negParser :: Parser Expr
negParser = do
  pos <- getPosition
  reservedOp "-"
  expr <- exprParser
  return $ Neg pos expr

callParser :: Parser Expr
callParser = do
  pos <- getPosition
  funcName <- idExprParser
  args <- parens $ commaSep exprParser
  return $ Call pos funcName args

newArrParser :: Parser Expr
newArrParser = do
  pos <- getPosition
  arrType <- typeIdParser
  size <- brackets exprParser
  reserved "of"
  initVal <- exprParser
  return $ NewArr { arrayType=arrType, arraySize=size, arrayInit=initVal, arrayPos=pos }

fieldInitParser :: Parser (Id, Expr)
fieldInitParser = do
    ident <- identifier
    reservedOp "="
    expr <- exprParser
    return (ident, expr)
        
newRecParser :: Parser Expr
newRecParser = do
  pos <- getPosition
  recType <- typeIdParser
  fieldInits <- braces $ commaSep  fieldInitParser
  return $ NewRec pos recType fieldInits

assignParser :: Parser Expr
assignParser = do
  pos <- getPosition
  var <- lValueParser
  reservedOp ":="
  expr <- exprParser
  return $ Assign pos var expr

ifParser :: Parser Expr
ifParser = do
  pos <- getPosition
  reserved "if"
  ifT <- exprParser
  reserved "then"
  thenE <- exprParser
  elseE <- optionMaybe $ do { reserved "else";
                                 elseE <- exprParser;
                                 return elseE }
  return $ If { ifTest=ifT, thenExpr=thenE, elseExpr=elseE, ifPos=pos }

whileParser :: Parser Expr
whileParser = do
  pos <- getPosition
  reserved "while"
  test <- exprParser
  reserved "do"
  body <- exprParser
  return $ While {whileTest=test, whileBody=body, whilePos=pos}
         
forParser :: Parser Expr
forParser = do
  pos <- getPosition
  reserved "for"
  ident <- identifier
  reservedOp ":="
  idExpr <- exprParser
  reserved "to"
  toE <- exprParser
  reserved "do"
  doE <- exprParser
  return $ For { forVarName=ident, forVarExpr=idExpr, toExpr=toE, doExpr=doE 
               , forPos=pos }

letParser :: Parser Expr
letParser = do
  pos <- getPosition
  reserved "let"
  decls <- declParser `sepBy` whiteSpace
  reserved "in"
  body <- semiSep exprParser
  reserved "end"
  return $ Let { letDecls=decls, letBody=body, letPos=pos}

makeOpParser :: Op -> Parser (Expr -> Expr -> Expr)
makeOpParser op = do 
  pos <- getPosition 
  reservedOp $ operatorMap Map.! op
  return $ InfixOp pos op

operators :: [[Operator String () Data.Functor.Identity.Identity Expr]]
operators = [ [Infix (makeOpParser Mult) AssocLeft]
            , [Infix (makeOpParser Div) AssocLeft]
            , [Infix (makeOpParser Add) AssocLeft]
            , [Infix (makeOpParser Sub) AssocLeft]
            , [Infix (makeOpParser And) AssocLeft]
            , [Infix (makeOpParser Or) AssocLeft]
            , [Infix (makeOpParser Eq) AssocLeft]
            , [Infix (makeOpParser NE) AssocLeft]
            , [Infix (makeOpParser Greater) AssocLeft]
            , [Infix (makeOpParser Less) AssocLeft]
            , [Infix (makeOpParser GreaterEq) AssocLeft]
            , [Infix (makeOpParser LessEq) AssocLeft]
            ]

exprParser :: Parser Expr
exprParser = buildExpressionParser operators genExprParser

genExprParser :: Parser Expr
genExprParser = 
    -- Parses LValues, Funcalls, New Array, Assignment.
    idAndExprParser 
    <|> do { pos <- getPosition; (reserved "nil"); return $ Nil pos }
    <|> do { pos <- getPosition; val <- natural; return $ IntConst pos val }
    <|> do { pos <- getPosition; val <- stringLiteral; return $ StringLit pos val }
    <|> seqExprParser
    <|> negParser
    <|> callParser
    <|> ifParser
    <|> whileParser
    <|> forParser
    <|> do { pos <- getPosition; (reserved "Break"); return $ Break pos }
    <|> letParser

readExpr :: String -> String
readExpr input = 
    case parse exprParser lang input of
      Left err -> show err
      Right val -> show val

--------------
-- Exported --
--------------
parseProg :: String -> String -> Prog
parseProg progName input =
    case parse (whiteSpace >> exprParser) progName input of
      Left err -> error $ show err
      Right prog -> prog
