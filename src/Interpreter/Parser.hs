module Interpreter.Parser (
  parse_str,
  ParseResult
) where
import Interpreter.Defs
import Interpreter.Primitives
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as Map

type ParseResult = Either (ParseError (Token String) Dec) [TLD]

--parse program from string
parse_str :: String -> ParseResult
parse_str s = parse lang_parser "<none>" s

--parser of the entire language
lang_parser :: Parser [TLD]


data Ass = AssL | AssR deriving (Eq, Show)

data Op = Op Int Ass VarE deriving Show -- precedence, associativity, function name




op_map :: Map.Map String Op
op_map = Map.fromList [
  ("$", Op 0 AssR ("__special__lambda")),
  ("||", Op 2 AssL (builtin_prefix ++ "or")),
  ("&&", Op 3 AssL (builtin_prefix ++ "and")),
  ("==", Op 4 AssL (builtin_prefix ++ "eq")),
  ("/=", Op 4 AssL (builtin_prefix ++ "neq")),
  ("<=", Op 4 AssL (builtin_prefix ++ "le")),
  (">=", Op 4 AssL (builtin_prefix ++ "ge")),
  ("<", Op 4 AssL (builtin_prefix ++ "lt")),
  (">", Op 4 AssL (builtin_prefix ++ "gt")),
  ("+", Op 6 AssL (builtin_prefix ++ "add")),
  ("-", Op 6 AssL (builtin_prefix ++ "sub")),
  ("*", Op 7 AssL (builtin_prefix ++ "mul")),
  ("/", Op 7 AssL (builtin_prefix ++ "div")),
  ("%", Op 7 AssL (builtin_prefix ++ "mod")),
  ("", Op 10 AssL ("__special__lambda"))]



reserved :: [String]
sc :: Parser ()
rword :: String -> Parser ()
rop :: String -> Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
parens :: Parser a -> Parser a
integer :: Parser Int
boolean :: Parser Bool
l_identifier :: Parser String
u_identifier :: Parser String
operator :: Parser Op
expr :: Parser Exp
expr_single :: Parser Exp
e_lambda :: Parser Exp
e_let :: Parser Exp
e_rec :: Parser Exp
e_if :: Parser Exp
apply_op :: Op -> Exp -> Exp -> Exp
expr_conversion :: [Exp] -> [Op] -> [(Op, Exp)] -> Exp
named_exp :: Parser (VarE, Exp)
toplevel_def :: Parser TLD

reserved = ["def", "data", "type", "True", "False", "let", "rec", "match", "with", "in", "if",
  "then", "else", "\\", "->", "="]

sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

rword s = try $ string s *> notFollowedBy (alphaNumChar <|> (char '_')) *> sc

rop s = try $ string s *> notFollowedBy (oneOf "=-.:/\\~<>!@$%^&*()+{}|?") *> sc

lexeme = L.lexeme sc

symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

integer = fromInteger <$> lexeme L.integer

boolean = (rword "True" *> pure True) <|> (rword "False" *> pure False)

l_identifier = try $ do
  s <- lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> (char '_'))
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ " cannot be an identifier"
    else return s

u_identifier = try $ do
  s <- lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> (char '_'))
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ " cannot be an identifier"
    else return s

operator = try $ do
  s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*+{}|?") -- reserved: [](),_"'`
  if s `elem` reserved
    then fail $ "forbidden keyword symbol: " ++ show s ++ " cannot be an operator"
    else case Map.lookup s op_map of
      Just x  -> return x
      Nothing -> fail $ "no such operator: " ++ show s

expr = do
  h <- expr_single
  t <- many (try $ (,) <$> operator <*> expr_single)
  return $ expr_conversion [h] [] t -- TODO: stack expr parsing algorithm

expr_single =
      (EData . DBool) <$> boolean
  <|> (EData . DInt) <$> integer
  <|> EVar <$> l_identifier
  <|> e_lambda
  <|> e_let
  <|> e_rec
  <|> e_if
  <|> parens expr

e_lambda = do
  rop "\\"
  var <- l_identifier
  rop "->"
  e <- expr
  return $ ELambda var e

e_let = do
  rword "let"
  var <- l_identifier
  rop "="
  e1 <- expr
  rword "in"
  e2 <- expr
  return $ ELet var e1 e2

e_rec = do
  rword "rec"
  var <- l_identifier
  rop "="
  e1 <- expr
  rword "in"
  e2 <- expr
  return $ ELetRec var e1 e2

e_if = do
  rword "if"
  cond <- expr
  rword "then"
  e1 <- expr
  rword "else"
  e2 <- expr
  return $ EApply (EApply (EApply (EVar $ builtin_prefix ++ "if") cond) e1) e2

apply_op (Op _ _ "__special__lambda") e1 e2 = EApply e1 e2

apply_op (Op _ _ name) e1 e2 = EApply (EApply (EVar name) e1) e2


expr_conversion [e] [] [] = e

expr_conversion (h2:h1:t) (o:to) [] = expr_conversion ((apply_op o h1 h2):t) to []

expr_conversion output [] ((o,e):to) = expr_conversion (e:output) [o] to

expr_conversion
  output  @(h2:h1                 :t)
  ops     @(ho@(Op pr1 _ _)       :to)
  input   @((o@(Op pr2 as _), h3) :ti) =
  if (as == AssL && pr2 <= pr1) || (as == AssR && pr2 < pr1)
    then expr_conversion ((apply_op ho h1 h2):t) to input
    else expr_conversion (h3:output) (o:ops) ti

expr_conversion _ _ _ = undefined -- should never happen


named_exp = do
  rword "def"
  var <- l_identifier
  rop "="
  e <- expr
  return (var, e)

toplevel_def =
      (uncurry NamedExp) <$> named_exp

lang_parser = between sc eof (some toplevel_def)
