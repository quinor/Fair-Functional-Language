module Interpreter.Parser (
  parseStr,
  ParseResult
) where
import Interpreter.Defs
import Interpreter.Primitives (builtinPrefix)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as Map

type ParseResult = Either (ParseError (Token String) Dec) [TLD]

--parse program from string
parseStr :: String -> ParseResult
parseStr = parse langParser "<none>"

--parser of the entire language
langParser :: Parser [TLD]


data Ass = AssL | AssR deriving (Eq, Show)

data Op = Op Int Ass VarE deriving Show -- precedence, associativity, function name




opMap :: Map.Map String Op
opMap = Map.fromList [
  ("$", Op 0 AssR "__special__lambda"),
  ("||", Op 2 AssL $ builtinPrefix ++ "or"),
  ("&&", Op 3 AssL $ builtinPrefix ++ "and"),
  ("==", Op 4 AssL $ builtinPrefix ++ "eq"),
  ("/=", Op 4 AssL $ builtinPrefix ++ "neq"),
  ("<=", Op 4 AssL $ builtinPrefix ++ "le"),
  (">=", Op 4 AssL $ builtinPrefix ++ "ge"),
  ("<", Op 4 AssL $ builtinPrefix ++ "lt"),
  (">", Op 4 AssL $ builtinPrefix ++ "gt"),
  ("+", Op 6 AssL $ builtinPrefix ++ "add"),
  ("-", Op 6 AssL $ builtinPrefix ++ "sub"),
  ("*", Op 7 AssL $ builtinPrefix ++ "mul"),
  ("/", Op 7 AssL $ builtinPrefix ++ "div"),
  ("%", Op 7 AssL $ builtinPrefix ++ "mod"),
  ("", Op 10 AssL "__special__lambda")]



reserved :: [String]
sc :: Parser ()
rword :: String -> Parser ()
rop :: String -> Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
parens :: Parser a -> Parser a
integer :: Parser Int
boolean :: Parser Bool
lIdentifier :: Parser String
uIdentifier :: Parser String
operator :: Parser Op
expr :: Parser Exp
exprSingle :: Parser Exp
eLambda :: Parser Exp
eLet :: Parser Exp
eRec :: Parser Exp
eIf :: Parser Exp
applyOp :: Op -> Exp -> Exp -> Exp
exprConversion :: [Exp] -> [Op] -> [(Op, Exp)] -> Exp
namedExp :: Parser (VarE, Exp)
toplevelDef :: Parser TLD

reserved = ["def", "data", "type", "True", "False", "let", "rec", "match", "with", "in", "if",
  "then", "else", "\\", "->", "="]

sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

rword s = try $ string s *> notFollowedBy (alphaNumChar <|> char '_') *> sc

rop s = try $ string s *> notFollowedBy (oneOf "=-.:/\\~<>!@$%^&*()+{}|?") *> sc

lexeme = L.lexeme sc

symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

integer = fromInteger <$> lexeme L.integer

boolean = (rword "True" *> pure True) <|> (rword "False" *> pure False)

lIdentifier = try $ do
  s <- lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ " cannot be an identifier"
    else return s

uIdentifier = try $ do
  s <- lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ " cannot be an identifier"
    else return s

operator = try $ do
  s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*+{}|?") -- reserved: [](),_"'`
  if s `elem` reserved
    then fail $ "forbidden keyword symbol: " ++ show s ++ " cannot be an operator"
    else case Map.lookup s opMap of
      Just x  -> return x
      Nothing -> fail $ "no such operator: " ++ show s

expr = do
  h <- exprSingle
  t <- many (try $ (,) <$> operator <*> exprSingle)
  return $ exprConversion [h] [] t -- TODO: stack expr parsing algorithm

exprSingle =
      (EData . DBool) <$> boolean
  <|> (EData . DInt) <$> integer
  <|> EVar <$> lIdentifier
  <|> eLambda
  <|> eLet
  <|> eRec
  <|> eIf
  <|> parens expr

eLambda = do
  rop "\\"
  vars <- (many lIdentifier)
  rop "->"
  e <- expr
  return $ foldr ELambda e vars

letDecls :: Parser [(VarE, Exp)]
letDecls = some $ do
  v <- lIdentifier
  rop "="
  e <- expr
  return (v, e)

eLet = do
  rword "let"
  vars <- letDecls
  rword "in"
  e2 <- expr
  return $ ELet vars e2

eRec = do
  rword "rec"
  vars <- letDecls
  rword "in"
  e2 <- expr
  return $ ELetRec vars e2

eIf = do
  rword "if"
  cond <- expr
  rword "then"
  e1 <- expr
  rword "else"
  e2 <- expr
  return $ EApply (EApply (EApply (EVar $ builtinPrefix ++ "if") cond) e1) e2

applyOp (Op _ _ "__special__lambda") e1 e2 = EApply e1 e2

applyOp (Op _ _ name) e1 e2 = EApply (EApply (EVar name) e1) e2


exprConversion [e] [] [] = e

exprConversion (h2:h1:t) (o:to) [] = exprConversion (applyOp o h1 h2:t) to []

exprConversion output [] ((o,e):to) = exprConversion (e:output) [o] to

exprConversion
  output  @(h2:h1                 :t)
  ops     @(ho@(Op pr1 _ _)       :to)
  input   @((o@(Op pr2 as _), h3) :ti) =
  if (as == AssL && pr2 <= pr1) || (as == AssR && pr2 < pr1)
    then exprConversion (applyOp ho h1 h2:t) to input
    else exprConversion (h3:output) (o:ops) ti

exprConversion _ _ _ = undefined -- should never happen


namedExp = do
  rword "def"
  var <- lIdentifier
  rop "="
  e <- expr
  return (var, e)

toplevelDef =
      uncurry NamedExp <$> namedExp

langParser = between sc eof (some toplevelDef)
